return function(parser)
	local go = parser.go

	-- error reporting
	local exprerr, expect = parser.exprerr, parser.expect

	-- basic precedence levels
	local p = parser.precedence
	local prec_above = parser.prec_above

	prec_above(p.primary, "power")
	prec_above(p.power, "unary")
	prec_above(p.unary, "mult")
	prec_above(p.mult, "add")
	prec_above(p.add, "concat")
	prec_above(p.concat, "comp")
	prec_above(p.comp, "equal")
	prec_above(p.equal, "and_")
	prec_above(p.and_, "or_")
	prec_above(p.or_, "assign")

	-- prefix
	local pre = parser.pre

	local function literal(tok)
		go(1)
		return { [0] = tok, tok[2], tok[3] }
	end
	pre["int"] = literal
	pre["str"] = literal
	pre["name"] = literal

	pre["_G"] = function(tok)
		go(1)
		return { [0] = tok, "name", "_G" }
	end

	local function unary(t)
		return function(tok)
			go(1)
			local right = parser.expr(p.unary)
			return { [0] = tok, t, right }
		end
	end
	pre["-"] = unary("unm")
	pre["#"] = unary("len")
	pre["!"] = unary("not")

	pre["return"] = function(tok)
		go(1)
		local right = parser.expr()
		return { [0] = tok, "return", right }
	end

	pre["("] = function(p0) -- )
		go(1)
		local e = parser.expr()
		local p1 = expect(parser.tokens[go(1)], ")", ")") --((
		e["p0"] = e["p0"] or p0 -- save parens tokens for accurate error reporting (see: getrange)
		e["p1"] = e["p1"] or p1
		return e
	end

	pre["["] = function(tok) -- ]
		go(1)

		local tbl = { [0] = tok, "table" }
		if parser.tokens[go()][2] == "]" then -- [
			go(1)
			return tbl
		end

		local after
		repeat
			local e = parser.expr()
			after = parser.tokens[go(1)]

			if after[2] == ":" then
				e = { [0] = after, "tableindex", e, parser.expr() }
				after = parser.tokens[go(1)]
			end

			tbl[#tbl+1] = e
		until after[2] ~= ","
		expect(after, "]", "']' or ',' after table item") -- [[
		tbl["b1"] = after

		return tbl
	end

	-- infix
	local post = parser.post

	local function leftrec(t, prec)
		return {
			prec = prec,
			func = function(tok, left)
				local right = parser.expr(prec)
				return { [0] = tok, t, left, right }
			end
		}
	end

	local function rightrec(t, prec)
		return {
			prec = prec,
			func = function(tok, left)
				local right = parser.expr(prec_above(prec))
				return { [0] = tok, t, left, right }
			end
		}
	end

	post["["] = { --]
		prec = p.primary,
		func = function(tok, left)
			local e = parser.expr()
			expect(parser.tokens[go(1)], "]", "]") -- [[

			return { [0] = tok, "idx", left, e }
		end
	}

	post["("] = { --)
		prec = p.primary,
		func = function(tok, left)
			local tbl = { [0] = tok, "call", left }
			local after

			if parser.tokens[go()][2] == ")" then -- (
				go(1)
				return tbl
			end

			repeat
				tbl[#tbl+1] = parser.expr()
				after = parser.tokens[go(1)]
			until after[2] ~= ","
			expect(after, ")", "')' or ',' after function call argument") -- ((

			return tbl
		end
	}

	post["."] = {
		prec = p.primary,
		func = function(tok, left)
			local name = parser.tokens[go(1)]
			expect(name, "name", "an identifier")
			return { [0] = tok, "dotidx", left, { [0] = name, "name", name[3] } }
		end
	}

	post["->"] = {
		prec = p.primary,
		func = function(tok, left)
			local name = parser.tokens[go(1)]
			expect(name, "name", "an identifier")
			expect(parser.tokens[go(1)], "(", "'(' for method call") -- ))

			local tbl = { [0] = tok, "selfcall", left, name[3] }
			local after

			if parser.tokens[go()][2] == ")" then -- (
				go(1)
				return tbl
			end

			repeat
				tbl[#tbl+1] = parser.expr()
				after = parser.tokens[go(1)]
			until after[2] ~= ","
			expect(after, ")", "')' or ',' after function call argument") -- ((

			return tbl
		end
	}

	-- arithmetic
	post["+"] = leftrec("add", p.add)
	post["-"] = leftrec("sub", p.add)
	post["*"] = leftrec("mul", p.mult)
	post["/"] = leftrec("div", p.mult)
	post["^"] = rightrec("pow", p.power)

	post[".."] = rightrec("cat", p.concat)

	post["<"] = leftrec("lt", p.comp)
	post[">"] = leftrec("gt", p.comp)
	post["<="] = leftrec("le", p.comp)
	post[">="] = leftrec("ge", p.comp)
	post["=="] = leftrec("eq", p.comp)
	post["!="] = leftrec("ne", p.comp)

	post["or"] = leftrec("or", p.or_)
	post["and"] = leftrec("and", p.and_)

	-- right-rec + check lhs
	local assigntargets = { idx = true, dotidx = true, name = true }
	post["="] = {
		prec = p.assign,
		func = function(tok, left)
			if not assigntargets[left[1]] then
				exprerr(left, tok[0], "Left side of an assignment must be an identifier or field")
			end

			local right = parser.expr(prec_above(p.assign))
			return { [0] = tok, "assign", left, right }
		end
	}

	-- block expressions & statements
	local function block(tok)
		if tok then
			go(1)
		end

		local tbl = { [0] = tok, "block" }
		local semiend

		while true do
			local tok2 = parser.tokens[go()]
			while tok2[2] == ";" do
				semiend = tok2
				tok2 = parser.tokens[go(1)+1]
			end
			if tok2[2] == "}" or (not tok and not tok2[2]) then --{
				go(1)
				break
			end

			-- the only real statement in all of daul, so i feel it's
			-- acceptable to just use an if statement here instead of a whole
			-- table of statements or whatever
			if tok2[2] == "var" or tok2[2] == "val" then
				go(1)

				local name = expect(parser.tokens[go(1)], "name", "an identifier")

				-- TODO: declaration without value
				expect(parser.tokens[go(1)], "=", "=")

				local right = parser.expr(prec_above(p.assign))

				semiend = false
				local e = { [0] = tok2, "local", { name[3] }, right }
				if tok2[2] == "val" then
					e = { [0] = tok2, "const", e }
				end
				tbl[#tbl+1] = e
			else
				semiend = false
				local e = parser.expr()
				tbl[#tbl+1] = e
			end
		end

		local lasttok = parser.tokens[go()-1]
		if tok then -- see: parens above; getrange
			tbl["b1"] = lasttok
		end
		if semiend ~= false then
			tbl[#tbl+1] = { [0] = semiend, "nil" }
		end

		return tbl
	end

	pre["{"] = block --}

	pre["\\"] = function(tok)
		go(1)
		local params = {}
		local tbl = { [0] = tok, "function", params }

		tok = parser.tokens[go()]
		if tok[2] == "name" then -- parse argument names
			go(1)
			params[1] = tok[3]
			tok = parser.tokens[go(1)]
			while tok[2] == "," do
				tok = parser.tokens[go(1)]
				params[#params+1] = tok[3]
				tok = parser.tokens[go(1)]
			end
			expect(tok, "->", "-> after function arguments")
			tok = parser.tokens[go()]
		end

		if tok[2] == "{" then -- }
			tbl[3] = block(tok)
		else
			tbl[3] = { "block", parser.expr() }
		end

		return tbl
	end

	pre["while"] = function(tok)
		go(1)
		local tbl = { [0] = tok, "while" }

		tok = parser.tokens[go(1)]
		expect(tok, "(", "(") -- ))

		tbl[2] = parser.expr() -- cond

		tok = parser.tokens[go(1)]
		expect(tok, ")", ")") -- ((

		tok = parser.tokens[go()]
		if tok[2] == "{" then -- }
			tbl[3] = block(tok)
		else
			tbl[3] = { "block", parser.expr() }
		end

		return tbl
	end

	pre["if"] = function(tok)
		go(1)
		local tbl = { [0] = tok, "if" }

		tok = parser.tokens[go(1)]
		expect(tok, "(", "(") -- ))

		tbl[2] = parser.expr() -- cond

		tok = parser.tokens[go(1)]
		expect(tok, ")", ")") -- ((

		tok = parser.tokens[go()]
		if tok[2] == "{" then -- }
			tbl[3] = block(tok)
		else
			tbl[3] = { "block", parser.expr() }
		end

		if parser.tokens[go()][2] == "else" then
			tok = parser.tokens[go(1)+1]
			if tok[2] == "{" then -- }
				tbl[4] = block(tok)
			else
				tbl[4] = { "block", parser.expr() }
			end
		end

		return tbl
	end

	pre["for"] = function(tok)
		go(1)

		local names = {}
		local tbl = { [0] = tok, nil, names }
		expect(parser.tokens[go(1)], "(", "(") -- ))

		repeat
			names[#names+1] = expect(parser.tokens[go(1)], "name", "an identifier")
			after = parser.tokens[go(1)]
		until after[2] ~= ","

		local bodyidx = 4
		if after[2] == ":" then -- for-in
			tbl[1] = "forin"
			tbl[3] = parser.expr()
		else
			expect(after, "=", "':' or '='")
			tbl[1] = "for"
			bodyidx = 6

			tbl[3] = parser.expr()
			expect(parser.tokens[go(1)], ",", ",")
			tbl[4] = parser.expr()
			if parser.tokens[go()][2] == "," then
				go(1)
				tbl[5] = parser.expr()
			end

			-- TODO: multiple names -> nested loops
			tbl[2] = names[1]
		end

		expect(parser.tokens[go(1)], ")", ")") -- ((

		tok = parser.tokens[go()]
		if tok[2] == "{" then -- }
			tbl[bodyidx] = block(tok)
		else
			tbl[bodyidx] = { "block", parser.expr() }
		end

		return tbl
	end

	return block()
end

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
	pre["name"] = literal

	local function unary(t)
		return function(tok)
			go(1)
			local right = parser.expr(p.unary)
			return { [0] = tok, t, right }
		end
	end
	pre["-"] = unary("unm")
	pre["#"] = unary("len")

	pre["("] = function(p0)
		go(1)
		local e = parser.expr()
		local p1 = expect(parser.tokens[go(1)], ")", ")") --((
		e["p0"] = e["p0"] or p0 -- save parens tokens for accurate error reporting (see: getrange)
		e["p1"] = e["p1"] or p1
		return e
	end

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

			semiend = false
			local e = parser.expr()
			tbl[#tbl+1] = e
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

		tok = parser.tokens[go(1)]
		if tok[2] == "name" then -- parse argument names
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

	post["."] = {
		prec = p.primary,
		func = function(tok, left)
			local name = parser.tokens[go(1)]
			expect(name, "name", "an identifier")
			return { [0] = tok, "dotidx", left, { [0] = name, "name", name[3] } }
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
	post["=="] = leftrec("eq", p.comp)
	post["!="] = leftrec("ne", p.comp)
	post["<="] = leftrec("le", p.comp)
	post[">="] = leftrec("ge", p.comp)

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

	return block()
end

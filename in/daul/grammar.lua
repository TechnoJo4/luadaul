return function(parser)
	local go = parser.go

	-- basic precedence levels
	local p = parser.precedence
	local prec_below, prec_above = parser.prec_below, parser.prec_above

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

	pre["("] = function()
		go(1)
		local e = parser.expr()
		assert(parser.tokens[go(1)][2] == ")")
		return e
	end

	local function block(tok)
		if tok then go(1) end

		local tbl = { [0] = tok, "block" }
		local semiend = true

		while true do
			local tok2 = parser.tokens[go()]
			while tok2[2] == ";" do
				semiend = true
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

		if semiend then
			tbl[#tbl+1] = { "nil" }
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
			assert(tok[2] == "->")
			tok = parser.tokens[go()]
		end

		tbl[3] = block(tok)

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
			assert(parser.tokens[go(1)][2] == "]") --[
			return { [0] = tok, "idx", left, e }
		end
	}

	post["."] = {
		prec = p.primary,
		func = function(tok, left)
			local name = parser.tokens[go(1)]
			assert(name[2] == "name")
			return { [0] = tok, "dotidx", left, { [0] = name, "name", name[3] } }
		end
	}

	post["+"] = leftrec("add", p.add)
	post["-"] = leftrec("sub", p.add)
	post["*"] = leftrec("mul", p.mult)
	post["/"] = leftrec("div", p.mult)

	-- right-rec + check lhs
	local assigntargets = { idx = true, dotidx = true, name = true }
	post["="] = {
		prec = p.assign,
		func = function(tok, left)
			assert(assigntargets[left[1]])

			local right = parser.expr(prec_above(p.assign))
			return { [0] = tok, "assign", left, right }
		end
	}

	return block()
end

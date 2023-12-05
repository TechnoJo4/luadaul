-- generic common base for pratt parsers
-- currently only used by daul but could apply to others

local lerror = require("out.error")

return function(tokens, grammar, err)
	-- position in token stream
	local i = 1

	-- precedence levels: { [0] = n, [1] = below, [2] = above }
	local P = {}
	local p = { primary = P }

	-- regenerate the numeric ordering values
	local function prec_regen()
		local function traverse(node, n)
			if node[1] then
				n = traverse(node[1], n)
			end

			node[0] = n

			if node[2] then
				n = traverse(node[2], n+1)
			end

			return n+1
		end

		traverse(P, 0)
	end

	-- obtain new precedence levels (or give existing levels new names)
	local function prec_below(node, name)
		local r = node[1]
		if not r then
			r = {}
			node[1] = r
			prec_regen()
		end
		if name then
			p[name] = r
		end
		return r
	end

	local function prec_above(node, name)
		local r = node[2]
		if not r then
			r = {}
			node[2] = r
			prec_regen()
		end
		if name then
			p[name] = r
		end
		return r
	end

	-- pratt parsing
	local pre = {}
	local post = {}

	local function go(d)
		local _i = i
		if d then
			i = i + d
		end
		return _i
	end

	local function expr(prec)
		-- get rule according to token type. end of stream has a nil type.
		local tok = tokens[i]
		local rule = pre[tok[2] or "end"]
		if not rule then
			err(tok[0], tok[1], nil, "Unexpected token ", tok[2])
		end

		-- parse prefix expression
		local e = rule(tok)

		-- parse infix/postfix, according to precedence if given by the user
		if prec then
			while true do
				tok = tokens[i]
				rule = post[tok[2] or "end"]

				if not rule then break end

				if rule.prec[0] >= prec[0] then break end

				go(1)
				e = rule.func(tok, e)
			end
		else
			while true do
				tok = tokens[i]
				rule = post[tok[2] or "end"]

				if not rule then break end

				go(1)
				e = rule.func(tok, e)
			end
		end

		return e
	end

	local function expect(tok, t, what)
		if tok[2] ~= t then
			err(tok[0], tok[1], nil, "Excepted ", what, ", got ", tok[2])
		end
	end

	local function exprerr(exp, highlight, ...)
		local range = lerror.getrange(exp)
		err(range[0], range[1], highlight, ...)
	end

	-- give control to the specific language
	return grammar({
		tokens = tokens,
		precedence = p,
		prec_below = prec_below,
		prec_above = prec_above,
		pre = pre,
		post = post,
		go = go,
		expr = expr,
		err = err,
		expect = expect,
		exprerr = exprerr
	})
end

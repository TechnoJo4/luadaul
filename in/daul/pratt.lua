-- generic common base for pratt parsers
-- currently only used by daul but could apply to others

local stream = require("stream")

return function(tokens, grammar, err)
	-- position in token stream
	local i = 1

	-- precedence levels: { [0] = n, [1] = below, [2] = above }
	local P = {}
	local p = { primary = P }

	-- regenerate the numeric ordering values
	local function prec_regen()
		local function traverse(node, i)
			if node[1] then
				i = traverse(node[1], i)
			end

			node[0] = i

			if node[2] then
				i = traverse(node[2], i+1)
			end

			return i+1
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
		if d then i = i + d end
		return _i
	end

	local function expr(prec)
		-- get rule according to token type. end of stream has a nil type.
		local tok = tokens[i]
		local rule = pre[tok[2] or "end"]
		if not rule then
			error("unexpected token "..tok[2])
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
	})
end

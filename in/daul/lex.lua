local stream = require("stream")

-- tokens are { [0] = start, [1] = end, [2] = type, [3] = value? }

return function(str)
	local p = 1
	local eos = 1 + #str

	local function char(c)
		if string.sub(str, p, p) == c then
			local _p = p
			p = p + 1
			return { [0] = _p, _p, c }
		end
	end

	local function oper(s)
		local e = p + #s - 1
		if string.sub(str, p, e) == s then
			local _p = p
			p = e + 1
			return { [0] = _p, e, s }
		end
	end

	local function keyword(w)
		local e = p + #w
		if string.sub(str, p, e - 1) == w and not string.match(str, "^[_a-zA-Z0-9]", e) then
			local _p = p
			p = e
			return { [0] = _p, e - 1, w }
		end
	end

	local function pattern(t, patt, f)
		local v, np = string.match(str, patt, p)
		if v then
			if f then
				v, np = f(str, v, np)
			end
			v = { [0] = p, np - 1, t, v }
			p = np
		end
		return v
	end

	return stream.new(function()
		p = string.match(str, "^[ \t\r\n]*()", p) -- skip whitespace
		if p >= eos then return stream.END end

		return char("(") or char(")") -- braces
			or char("[") or char("]")
			or char("{") or char("}")
			or char("\\")
			or oper("==") or oper("!=") -- operators
			or oper("<=") or oper(">=")
			or oper("..") or oper("->")
			or char(".") or char("=")
			or char("<") or char(">")
			or char("+") or char("-")
			or char("*") or char("/")
			or char("^")
			or char(";") or char(",")
			or keyword("def")
			or keyword("var")
			or keyword("val")
			or keyword("while")
			or pattern("name", "^([_a-zA-Z][_a-zA-Z0-9]*)()")
			or pattern("int", "^([0-9]+)()")
			or pattern("str", "^\"()")
			or error("no such token ("..string.sub(str, p, p)..")")
	end)
end

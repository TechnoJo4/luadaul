-- error reporter creation

local red = "\27[31m"
local reset = "\27[0m"

return {
	getrange = require("pass.traverse") {
		call_with_raw_parent = true,

		init = function()
			return {}
		end,

		all = function(ir, r)
			if ir[0] then
				local t = ir[0]
				if not r[0] then
					r[0] = t[0]
					r[1] = t[1]
					return
				end

				if t[0] < r[0] then
					r[0] = t[0]
				end
				if t[1] > r[1] then
					r[1] = t[1]
				end
			end
		end,

		finalize = function(_, r)
			return r
		end
	},

	reporter = function(str)
		local nls = {1} -- positions of new lines
		for p in string.gmatch(str, "()\n") do
			nls[#nls+1] = p
		end
		if nls[#nls] ~= #str then
			nls[#nls+1] = #str
		end

		local numpad = #tostring(#nls) + 1
		local offset = numpad + 3

		-- first,last is a byte range
		return function(first, last, highlight, ...)
			-- find line; TODO binsearch
			local i = 1
			while nls[i+1] < first do
				i = i + 1
			end

			local lstart = string.match(str, "[ \t]*()", nls[i])
			local line = ""
			if first > lstart then
				line = string.sub(str, lstart, first - 1)
			end
			line = line .. red .. string.sub(str, first, last) .. reset
			if last < nls[i+1]-1 then
				line = line .. string.sub(str, last + 1, nls[i+1]-1)
			end

			local num = tostring(i)
			num = string.rep(" ", numpad - #num) .. num .. " | "
			print(num .. line)
			print(string.rep(" ", offset + first - lstart) .. string.rep("~", last - first + 1))
			print(table.concat({...}))

			os.exit(1) -- TODO
		end
	end
}

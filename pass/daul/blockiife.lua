-- allow the use of statements as expressions: part 2
-- transform blocks into immediately-invoked function expressions
-- TODO: do not transform if all elements are expressions

local stmts = require("pass.daul.stmts")
local returnlast = { ["root"] = true, ["function"] = true }

return require("pass.traverse") {
	call_with_raw_parent = true,

	["block"] = function(ir, recurse, parent, pi)
		-- flatten multiple levels of blocks into one
		local new = {"block"}

		local function flatten(n)
			for i=2,#n-1 do
				local v = n[i]
				if v[1] == "block" then
					flatten(v)
				else
					new[#new+1] = v
				end
			end
		end

		flatten(ir)
		new[#new+1] = ir[#ir]

		ir = new

		for i=2,#ir do
			recurse(ir, i)
		end

		-- return last value: special cases
		if returnlast[parent[1]] then
			ir[#ir] = { "return", ir[#ir] }
			parent[pi] = ir
			return
		end

		-- check parent type to see if no IIFE transform is required
		if stmts[parent[1]] or parent[1] == "function" then
			parent[pi] = ir
			return
		end

		-- return last value
		ir[#ir] = { "return", ir[#ir] }

		-- create IIFE
		parent[pi] = { "call", { "function", nil, ir } }
	end
}

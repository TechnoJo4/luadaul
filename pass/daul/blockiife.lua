-- allow the use of statements as expressions: part 2
-- transform blocks into immediately-invoked function expressions
-- TODO: do not transform if all elements are expressions?

local stmts = require("pass.daul.stmts")
local returnlast = { ["root"] = true, ["function"] = true }
local notransform = {
	["while"] = function(pi) return pi ~= 2 end
}

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

		while new[#new][1] == "block" do
			local b = new[#new]
			local last = b[#b]
			new[#new] = nil
			flatten(b)
			new[#new+1] = last
		end

		ir = new

		for i=2,#ir do
			recurse(ir, i)
		end

		-- special cases: return last value
		if returnlast[parent[1]] then
			ir[#ir] = { "return", ir[#ir] }
			parent[pi] = ir
			return
		end

		-- statements
		local check = notransform[parent[1]]
		if check and check(pi) then
			-- there are no other passes left that could take the last expr of
			-- the block; if it's just a name, discard it
			if ir[#ir][1] == "name" then
				ir[#ir] = nil
			elseif not stmts[ir[#ir][1]] then
				-- if it's not a statement but not just a name, wrap in id call
				ir[#ir] = { "call", { "name", "_" }, ir[#ir] }
			end

			parent[pi] = ir
			return
		end

		-- return last value
		ir[#ir] = { "return", ir[#ir] }

		-- create IIFE
		parent[pi] = { "call", { "function", nil, ir } }
	end
}

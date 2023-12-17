-- allow the use of statements as expressions
-- create a temporary local, add assignments to that local in all branches,
-- then make the original statement just use that local

local stmts = require("pass.daul.stmts")

return require("pass.traverse") {
	init = function()
		local counter = -1
		return function()
			counter = counter + 1
			return "_"..string.format("%X",counter)
		end
	end,

	finalize = function(ir)
		-- assumption: ir (root) will always be a block

		-- move everything 1 to the right
		for i=#ir,2,-1 do
			ir[i+1] = ir[i]
		end

		-- add the id function
		ir[2] = { "local", { "_" }, { "function", { "v" }, { "block", { "name", "v" } } } }

		return ir
	end,

	["return"] = function(ir, recurse, edit, gen)
		if not ir[2] then return end

		local new
		for i=2,#ir do
			recurse(ir, i)

			if stmts[ir[i]] then
				local name = gen()

				if new then
					local vars = new[2][2]
					vars[#vars+1] = name
				else
					new = { "block", { "local", { name } } }
				end

				local newi = #new+1
				new[newi] = { "assign", { "name", name }, ir[i] }
				recurse(new, newi)

				ir[i] = { "name", name }
			end
		end

		if new then
			new[#new+1] = ir
			edit(new)
		end
	end,

	["assign"] = function(ir, recurse, edit, gen)
		recurse(ir, 2)
		recurse(ir, 3)

		local vt = ir[3][1]
		if vt == "block" then
			local block = ir[3]
			if ir[2][1] == "name" then
				block[#block] = { "assign", ir[2], block[#block] }
				block[#block+1] = ir[2]
			else
				local name = gen()
				block[#block] = { "local", { name }, block[#block] }
				block[#block+1] = { "assign", ir[2], { "name", name } }
				block[#block+1] = { "name", name }
			end
			edit(ir[3])
		elseif stmts[vt] then
			if vt == "return" then -- return never gives a value, so we can discard
				edit(ir[3])
			end

			if vt == "assign" then
				if ir[3][2][1] == "name" then -- index = name = expr
					edit{ "block", ir[3], { "assign", ir[2], ir[3][2] }, ir[3][2] }
				elseif ir[2][1] == "name" then -- name = index = expr
					edit{ "block", { "assign", ir[2], ir[3][3] }, { "assign", ir[3][2], ir[2] }, ir[2] }
				else -- index = index = expr; we have to create a local
					local name = gen()
					edit{ "block",
						{ "local", { name }, ir[3][3] },
						{ "assign", ir[3][2], { "name", name } },
						{ "assign", ir[2], { "name", name } },
						{ "name", name } }
				end
			end
		else
			if ir[2][1] == "name" then
				edit({ "block", ir, ir[2] })
			elseif ir[3][1] == "name" then
				edit({ "block", ir, ir[3] })
			else
				local name = gen()

				edit{ "block",
					{ "local", { name }, ir[3] },
					{ "assign", ir[2], { "name", name } },
					{ "name", name } }
			end
		end
	end,

	["block"] = function(ir, recurse, edit, gen)
		-- this forces the last value of a block to be an expression, such that
		-- blockiife can either discard it (in its flattening phase) or create a
		-- return statement (in the iife phase)
		local lt = ir[#ir][1]
		if lt == "assign" then
			local a = ir[#ir]
			if a[2][1] == "name" then
				ir[#ir+1] = a[2]
			elseif a[3][1] == "name" then
				ir[#ir+1] = a[3]
			else
				local name = gen()
				ir[#ir] = { "local", { name }, a[3] }
				name = { "name", name }
				ir[#ir+1] = { "assign", a[2], name }
				ir[#ir+1] = name
			end
		end

		-- recurse
		for i=2,#ir do
			recurse(ir, i)
		end

		-- this wraps every non-statement expression in a call to id in order
		-- to bend lua to the user's will
		for i=2,#ir-1 do
			if not stmts[ir[i][1]] then
				ir[i] = { "call", { "name", "_" }, ir[i] }
			end
		end
	end,

	["while"] = function(ir, recurse, edit, gen)
		recurse(ir, 2)
		recurse(ir, 3)

		-- see comment in @block: we can now assume the last value of the body
		-- is an expression (and so valid as an assign's rhs)
		local name = gen()
		local body = ir[3]
		body[#body] = { "assign", { "name", name }, body[#body] }
		body[#body+1] = { "name", name }

		edit{ "block", { "local", { name } }, ir, { "name", name } }
	end
}

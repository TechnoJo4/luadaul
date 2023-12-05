-- base table; traverses the entire tree without any operation
local function unop(ir, recurse)
	recurse(ir, 2)
end

local function binop(ir, recurse)
	recurse(ir, 2)
	recurse(ir, 3)
end

local function all(ir, recurse)
	for i=2,#ir do
		recurse(ir, i)
	end
end

local function fun(ir, recurse)
	recurse(ir, 3)
end

local function all3(ir, recurse)
	for i=3,#ir do
		recurse(ir, i)
	end
end

local base = {
	unm = unop, len = unop,
	add = binop, sub = binop, mul = binop, div = binop,
	assign = binop,
	block = all,
	["local"] = all3,
	["function"] = fun, call = all,
}

-- create traverse function from table {[IR type]: func}
return function(tbl)
	for k,v in pairs(base) do
		if not tbl[k] then
			tbl[k] = v
		end
	end

	local rawparent = tbl.call_with_raw_parent

	local recurse, state
	recurse = function(ir, i)
		local c = ir[i]
		local f = tbl[c[1]]

		if tbl.all then
			tbl.all(c, state)
		end

		if f then
			if rawparent then
				f(c, recurse, ir, i, state)
			else
				f(c, recurse, function(new)
					ir[i] = new
				end, state)
			end
		end
	end

	return function(ir)
		if tbl.init then
			state = tbl.init()
		end

		local c = ir
		local f = tbl[c[1]]

		if tbl.all then
			tbl.all(c, state)
		end

		if f then
			if rawparent then
				local t = {"root", c}
				f(ir, recurse, t, 2, state)
				c = t[2]
			else
				f(ir, recurse, function(new)
					c = new
				end, state)
			end
		end

		return tbl.finalize and tbl.finalize(c, state) or c
	end
end
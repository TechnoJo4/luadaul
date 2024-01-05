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

local function fori(ir, recurse)
	recurse(ir, 3)
	recurse(ir, 4)
	if ir[5] then
		recurse(ir, 5)
	end
	recurse(ir, 6)
end

local base = {
	unm = unop, len = unop, ["not"] = unop,
	["or"] = binop, ["and"] = binop, cat = binop,
	add = binop, sub = binop, mul = binop, div = binop, pow = binop,
	lt = binop, gt = binop, le = binop, ge = binop, eq = binop, ne = binop,
	assign = binop,
	block = all, ["function"] = fun, call = all,
	["local"] = all3, ["const"] = unop,
	["while"] = binop, ["for"] = fori, ["forin"] = binop,
	["idx"] = binop, ["dotidx"] = unop,
	["table"] = all, ["tableindex"] = binop
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
		if not c then error("attempt to recurse into nonexistent node") end
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
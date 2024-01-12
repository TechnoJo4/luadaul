-- this makes no effort to minimize correctly or to verify that the input will
-- even create valid lua output.

--[[
allowed IR elements:
	literals, pasted as-is: (t x) where t is one of
		int str name
	
	binops: (f a b) where f is one of
		add sub mul div
		assign

	call: (call f ...params)

	local: (local (...names) ...values)

	block: (block ...stmts)

	function: (function (...params)? body) where body is a block

	return: (return ...exprs)

	table: (table ...items) where item can be
		(tableindex key value)
		value
]]

local literal = {
	["int"] = true, ["str"] = true, ["name"] = true
}

local binops = {
	["add"] = "+", ["sub"] = "-", ["mul"] = "*", ["div"] = "/", ["cat"] = "..",
	["lt"] = "<", ["gt"] = ">", ["le"] = "<=", ["ge"] = ">=", ["eq"] = "==", ["ne"] = "~=",
	["or"] = " or ", ["and"] = " and "
}

local function r(t, i, n)
	local tmp

	local nt = n[1]

	if literal[nt] then
		t[i] = n[2]
		return i + 1
	end

	if nt == "nil" then
		t[i] = "nil"
		return i + 1
	end

	if nt == "true" then
		t[i] = "true"
		return i + 1
	end

	if nt == "false" then
		t[i] = "false"
		return i + 1
	end

	if nt == "assign" then
		i = r(t, i, n[2])
		t[i] = "="
		i = r(t, i+1, n[3])

		return i
	end

	if nt == "dotidx" then
		t[i] = "(" -- )
		i = r(t, i+1, n[2])
		t[i] = ")." -- (
		t[i+1] = n[3][2]

		return i + 2
	end

	if nt == "idx" then
		t[i] = "(" -- )
		i = r(t, i+1, n[2])
		t[i] = ")[" -- ](
		i = r(t, i+1, n[3])
		t[i] = "]" -- [

		return i + 1
	end

	tmp = binops[nt]
	if tmp then
		t[i] = "("
		i = r(t, i+1, n[2])
		t[i] = tmp
		i = r(t, i+1, n[3])
		t[i] = ")"

		return i + 1
	end

	if nt == "len" then
		t[i] = "#("
		i = r(t, i+1, n[2])
		t[i] = ")"

		return i + 1
	end

	if nt == "unm" then
		t[i] = "-("
		i = r(t, i+1, n[2])
		t[i] = ")"

		return i + 1
	end

	if nt == "not" then
		t[i] = "not("
		i = r(t, i+1, n[2])
		t[i] = ")"

		return i + 1
	end

	if nt == "call" then
		i = r(t, i, n[2])
		t[i] = "("

		if n[3] then
			for j=3,#n do
				-- on the first iteration, +1 from the open parens
				-- on all others, +1 from last param's comma
				i = r(t, i+1, n[j])

				-- extraneous last comma overridden by close parens after loop
				t[i] = ","
			end
		else
			i = i + 1
		end

		t[i] = ")"
		return i+1
	end

	if nt == "selfcall" then
		i = r(t, i, n[2])
		t[i] = ":"
		t[i+1] = n[3]
		t[i+2] = "("
		i = i + 2

		if n[4] then
			for j=4,#n do
				-- on the first iteration, +1 from the open parens
				-- on all others, +1 from last param's comma
				i = r(t, i+1, n[j])

				-- extraneous last comma overridden by close parens after loop
				t[i] = ","
			end
		else
			i = i + 1
		end

		t[i] = ")"
		return i+1
	end

	if nt == "return" then
		t[i] = "return "

		for j=2,#n-1 do
			-- on the first iteration, +1 from the return keyword
			-- on all others, +1 from last expression's comma
			i = r(t, i+1, n[j])
			t[i] = ","
		end
		if n[2] then
			-- last expression: +1 from either return keyword or comma before
			i = r(t, i+1, n[#n])
		end

		return i
	end

	if nt == "const" then
		return r(t, i, n[2])
	end

	if nt == "local" then
		t[i] = "local "
		t[i+1] = table.concat(n[2], ",")
		i = i + 2

		if n[3] and #n[3] ~= 0 then
			t[i] = "="
			for j=3,#n-1 do
				-- on the first iteration, +1 from the equal
				-- on all others, +1 from last expression's comma
				i = r(t, i+1, n[j])
				t[i] = ","
			end
			-- last expression: +1 from either equal or comma before
			i = r(t, i+1, n[#n])
		end

		return i
	end

	if nt == "block" then
		i = r(t, i, n[2])
		t[i] = ";"
		for j=3,#n do
			i = r(t, i+1, n[j])
			t[i] = ";"
		end

		return i
	end

	if nt == "function" then
		t[i] = "(function("
		i = i + 1

		if n[2] and #n[2] ~= 0 then
			for j=1,#n[2]-1 do
				t[i] = n[2][j]
				t[i+1] = ","
				i = i + 2
			end
			t[i] = n[2][#n[2]]
			i = i + 1
		end

		t[i] = ")"

		local block = n[3]
		for j=2,#block do
			-- on the first iteration, +1 from the close parens
			-- on all others, +1 from last statement's semicolon
			i = r(t, i + 1, block[j])
			t[i] = ";"
		end

		-- see comment in loop
		t[i+1] = "end)"
		return i+2
	end

	if nt == "while" then
		t[i] = "while "
		i = r(t, i+1, n[2])
		t[i] = " do "
		i = r(t, i+1, n[3])
		t[i] = ";end"

		return i+1
	end

	if nt == "if" then
		t[i] = "if "
		i = r(t, i+1, n[2])
		t[i] = " then "
		i = r(t, i+1, n[3])
		if n[4] then
			t[i] = ";else "
			i = r(t, i+1, n[4])
		end
		t[i] = ";end"

		return i+1
	end

	if nt == "for" then
		t[i] = "for "
		t[i+1] = n[2][3]
		t[i+2] = "="
		i = r(t, i+3, n[3])
		t[i] = ","
		i = r(t, i+1, n[4])
		if n[5] then
			t[i] = ","
			i = r(t, i+1, n[5])
		end

		t[i] = " do "
		i = r(t, i+1, n[6])
		t[i] = ";end"

		return i+1
	end

	if nt == "forin" then
		t[i] = "for "
		i = i + 1

		for j=1,#n[2] do
			t[i] = n[2][j][3]
			t[i+1] = ","
			i = i + 2
		end
		t[i-1] = " in " -- overrides last comma
		i = r(t, i, n[3])
		t[i] = " do "
		i = r(t, i+1, n[4])
		t[i] = ";end"

		return i+1
	end

	if nt == "table" then
		t[i] = "{"
		if n[2] then
			for j=2,#n do
				i = r(t, i+1, n[j])
				t[i] = ","
			end
			t[i] = "}"
		else
			t[i] = "{}"
		end

		return i+1
	end

	if nt == "tableindex" then
		t[i] = "["
		i = r(t, i+1, n[2])
		t[i] = "]="
		i = r(t, i+1, n[3])

		return i
	end

	if type(nt) ~= "string" then
		error("lua output: fucked ir (got "..type(nt).." type: "..tostring(nt)..")")
	else
		error("lua output: unknown ir type ("..nt..")")
	end
end

if false then -- debug: check for holes before concat
	return function(ir)
		local tbl = {}
		r(tbl, 1, ir)
		
		local i = 1
		while true do
			if not tbl[i] then
				if not tbl[i+1] then
					print(string.format("end @ %d", i))
					break
				end

				print(string.format("hole @ %d: %s ?? %s", i, tbl[i-1], tbl[i+1]))
			end

			if type(tbl[i]) ~= "string" then
				print(string.format("%s @ %d: %s ?? %s", type(tbl[i]), i, tbl[i-1], tbl[i+1]))
			end

			i = i + 1
		end

		return table.concat(tbl, "")
	end
end

return function(ir)
	local tbl = {}
	r(tbl, 1, ir)
	return table.concat(tbl, "")
end

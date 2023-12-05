-- "pretty"-print daul IR
-- definitely slow but only used in debugging so who cares
local function irs(n)
	local function bin()
		return "(" .. irs(n[2]) .. " " .. n[1] .. " " .. irs(n[3]) .. ")"
	end

	local f = ({
		var = function() return "var" end,
		val = function() return "val" end,
		["nil"] = function() return "nil" end,
		int = function() return "i:"..n[2] end,
		name = function() return "`"..n[2] end,
		unm = function() return "-(" .. irs(n[2]) .. ")" end,
		add = bin,
		sub = bin,
		mul = bin,
		div = bin,
		assign = bin,
		["local"] = function()
			local s = "(local "

			s = s .. table.concat(n[2], ", ") .. " ="

			for i=3,#n do
				s = s .. " " .. irs(n[i])
			end

			return s .. ")"
		end,
		["function"] = function()
			local s = "(fun: "
			if n[2] then
				s = s .. table.concat(n[2], ", ") .. " -> "
			end

			s = s .. irs(n[3]) .. ")"

			return s
		end,
		["block"] = function()
			local s = "{"
			for i=2,#n do
				s = s .. irs(n[i]) .. "; "
			end

			return s .. "}"
		end,
		["call"] = function()
			local s = irs(n[2]) .. "@("
			for i=3,#n do
				s = s .. " " .. irs(n[i])
			end
			return s .. ")"
		end,
		["dotidx"] = function()
			return irs(n[2]) .. "." .. irs(n[3])
		end,
		["return"] = function()
			local s = "return ["
			for i=2,#n-1 do
				s = s .. irs(n[i]) .. ", "
			end
			if n[2] then
				s = s .. irs(n[#n])
			end

			return s .. "]"
		end,
	})[n[1]]

	if not f then
		print("irs:err", n[1])
		return "err"
	end

	return f()
end

return irs

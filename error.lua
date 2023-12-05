-- error reporter creation

return function(str)
	local nls = {1} -- positions of newlines
	for p in string.gmatch(str, "()\n") do
		nls[#nls+1] = p
	end
	if nls[#nls] ~= #str then
		nls[#nls+1] = #str
	end

	-- first,last is a byte range
	return function(first, last, reason, highlight)
		-- find line
		local i = 1
		while nls[i+1] < first do
			i = i + 1
		end

		-- TODO
	end
end

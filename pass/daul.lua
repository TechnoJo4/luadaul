-- combination of all IR passes necessary to get daul's IR into a state
-- appropriate for final output to lua source code

local stmtexpr = require("pass.daul.stmtexpr")
local blockiife = require("pass.daul.blockiife")

local outs = require("out.s")

return function(ir)
	ir = stmtexpr(ir)
	print(outs(ir))
	ir = blockiife(ir)

	return ir
end

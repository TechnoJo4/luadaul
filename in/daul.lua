local err = require("out.error")
local lex = require("in.daul.lex")
local pratt = require("in.daul.pratt")
local grammar = require("in.daul.grammar")

return function(str, reporter)
	return pratt(lex(str, reporter), grammar, reporter)
end

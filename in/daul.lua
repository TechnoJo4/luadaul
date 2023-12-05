local err = require("error")
local lex = require("in.daul.lex")
local pratt = require("in.daul.pratt")
local grammar = require("in.daul.grammar")

return function(str)
	return pratt(lex(str), grammar, err(str))
end
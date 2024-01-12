-- minimal main.lua for bootstrapping purposes
-- assumes last two arguments are source and target, ignores everything else

local inluvi, luvi = pcall(require, "luvi")
if inluvi then
	luvi.bundle.register("loader", "luviloader.lua")
	require("loader")
end

local argv = arg or {...}
local l = #argv
local fin = argv[l-1]
local fout = argv[l]

if not fin or not fout then
	print("no filename")
	return
else
	print(string.format("bootstrap: %s -> %s", fin, fout))
end

-- read file
local f = io.open(fin, "r")
local s = f:read("*a")
f:close()

-- load daul
local outs = require("out.s")
local daul = require("in.daul")
local err = require("out.error")
local pass = require("pass.daul")
local lua = require("out.lua")

-- compile
local reporter = err.reporter(s)
local e = daul(s, reporter)
e = pass(e, reporter)
s = lua(e)

-- write lua result
f = io.open(fout, "w")
f:write(s)
f:close()

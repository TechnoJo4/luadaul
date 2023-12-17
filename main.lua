local f = io.open("test.daul", "r")
local s = f:read("*a")
f:close()

print(s)

local outs = require("out.s")
local daul = require("in.daul")
local e = daul(s)
print(outs(e))

local pass = require("pass.daul")
e = pass(e)
print(outs(e))

print()

local lua = require("out.lua")
s = lua(e)
print(s)

f = io.open("_test.lua", "w")
f:write(s)
f:close()

print()
print('require("_test")...')
print(require("_test"))

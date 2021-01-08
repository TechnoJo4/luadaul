local parse = require("parse.parser")
local ir = require("emit.ir")
local lua51 = require("emit.lua51")

-- todo
if process.argv[2] then
    local f = io.open(process.argv[2])
    local source = f:read("*a")
    local parser = parse.new_parser(source)
    local irc = ir.new_irc()
    f:close()
    local v = parser:stmt()
    --print(v)
    --print(parser.lexer.get(1))
    irc:stmt(v)
    --print()
    for _,v in ipairs(irc.ir) do
        print(v)
    end
    local bcc = lua51.new_compiler(irc, true)
    local bc = bcc:compile_main("test")

    local f = io.open("test.luac", "wb")
    f:write(bc)
    f:close()
else
    error("argv[2]")
end

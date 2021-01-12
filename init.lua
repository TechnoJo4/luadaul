local T = require("parse.token")
local parse = require("parse.parser")
local ir = require("emit.ir")
local lua51 = require("emit.lua51")

setmetatable(_G, { __newindex=function(_, k) error("tried to create global", k) end })

-- warning: this is horrible
-- todo: make this mess not a mess
if process.argv[2] then
    local f = io.open(process.argv[2])
    local source = f:read("*a")
    local parser = parse.new_parser(source)
    local irc = ir.new_irc()
    f:close()
    while parser.lexer.get(1).type ~= T.EOF do
        local v = parser:stmt()
        --print(v)
        irc:stmt(v)
        while parser.lexer.get(1).type == T.Newline do
            parser.lexer.adv()
        end
    end
    print(irc)
    local bcc = lua51.new_compiler(irc, true)
    local bc = bcc:compile_main("test")

    local f = io.open("test.luac", "wb")
    f:write(bc)
    f:close()
else
    error("argv[2]")
end

local T = require("parse.token")
local parse = require("parse.parser")
local ir = require("emit.ir")
local lua51 = require("emit.lua51")

setmetatable(_G, { __newindex=function(_, k) error("tried to create global", k) end })

local argv
if process then -- luvi/luvit support
    for k,v in ipairs(process.argv) do
        if k > 1 then
            -- TODO: luvi packaged executables will not include the ./init.lua argument,
            -- so i guess i'll have to find a way to know whether to shift args or not
            argv[k-1] = v
        end
    end
else
    argv = {...}
end

-- warning: this is horrible
-- TODO: make this mess not a mess
if argv[1] then
    --local p = require("jit.p")
    --p.start()
    local f = io.open(argv[1])
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
    --p.stop()

    local f = io.open("test.luac", "wb")
    f:write(bc)
    f:close()
else
    error("argv[1]")
end

local T = require("parse.token")
local parse = require("parse.parser")
local ir = require("emit.ir")
local lua51 = require("emit.lua51")

-- warning: this is horrible and very temporary

setmetatable(_G, { __newindex=function(_, k) error("tried to create global " .. k) end })

local argv
if process then -- luvi/luvit support
    argv = {}
    for k,v in ipairs(process.argv) do
        if k > 1 then
            argv[k-1] = v
        end
    end
else
    argv = {...}
end

-- TODO: make this mess not a mess
if argv[1] then
    local f_in = argv[1]
    local f_out = "a.lua"
    if #f_in > 5 and f_in:sub(-5, -1) == ".daul" then
        -- TODO: .luac? .raw?
        f_out = f_in:sub(1, -6) .. ".lua"
    end

    local f = io.open(f_in)
    local source = f:read("*a")
    f:close()

    local parser = parse.new_parser(source)
    local irc = ir.new_irc()

    while parser.lexer.get(1).type ~= T.EOF do
        irc:stmt(parser:stmt())
    end

    local bcc = lua51.new_compiler(irc, true)
    f = io.open(f_out, "wb")
    f:write(bcc:compile_main("@"..f_in))
    f:close()
else
    -- no repl because compiler can't run on lua51 and no luajit emitter
    error("argv[1]")
end

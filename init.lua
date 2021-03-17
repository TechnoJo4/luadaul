local T = require("parse.token")
local parse = require("parse.parser")
local ir = require("ir.compile")
local lua = require("emit.luajit")

-- warning: this file is horrible and very temporary

local argv
-- selene: allow(undefined_variable)
if process then -- luvi/luvit support
    argv = {}
    for k,v in ipairs(process.argv) do
        if k > 1 then
            argv[k-1] = v
        end
    end
else
    argv = { ... }
end

-- TODO: make this mess not a mess
-- TODO: create a real arg parser
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

    local ptime = 0
    local itime = 0
    while parser.lexer.get(1).type ~= T.EOF do
        local t1 = os.clock()
        local stmt = parser:stmt()
        local t2 = os.clock()
        ptime = t2 - t1 + ptime
        irc:stmt(stmt)
        itime = os.clock() - t2 + itime
    end
    local ltime = parser.lexer.time()
    --print("Lexer   ", ltime)
    --print("Parser  ", ptime)
    --print("IR      ", itime)

    ltime = os.clock()
    local bcc = lua.new_compiler(irc, true)
    local bc = bcc:compile_main("@"..f_in)
    --print("Bytecode", os.clock() - ltime)
    f = io.open(f_out, "wb")
    f:write(bc)
    f:close()
else
    -- no repl because compiler can't run on lua51 and luajit emitter is incomplete
    error("argv[1]")
end

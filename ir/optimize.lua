local IR = require("ir/insts")

-- http://wiki.luajit.org/Optimizations

local fold = require("ir/opt/fold")
local bool_fold = require("ir/opt/bool_fold")
local concat_fold = require("ir/opt/concat_fold")

local function inst(ir, irc)
    ir = fold(ir, irc)
    ir = bool_fold(ir, irc)
    ir = concat_fold(ir, irc)

    return ir
end

local fix_consts = require("ir/opt/remove_unused_consts")

local function final(irc)
    fix_consts(irc)
end

return { inst=inst, final=final }

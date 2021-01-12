local IR = require("emit.irinsts")

local fold = {
    [IR.ADD] = function(a, b) return a + b end,
    [IR.SUB] = function(a, b) return a - b end,
    [IR.MUL] = function(a, b) return a * b end,
    [IR.DIV] = function(a, b) return a / b end,
    [IR.MOD] = function(a, b) return a % b end,
    [IR.POW] = function(a, b) return a ^ b end
}
local foldr = {
    [IR.ADD] = function(l, a, b)
        if l == IR.ADD or l == IR.SUB then
            return true, fold[l](a, b)
        end
        return false
    end,
    [IR.SUB] = function(l, a, b)
        if l == IR.ADD then
            return true, a - b
        elseif l == IR.SUB then
            return true, a + b
        end
        return false
    end,
    [IR.MUL] = function(l, a, b)
        if l == IR.MUL or l == IR.DIV then
            return true, fold[l](a, b)
        end
        return false
    end,
    [IR.DIV] = function(l, a, b)
        if l == IR.MUL then
            return true, a / b
        elseif l == IR.DIV then
            return true, a * b
        end
        return false
    end,
    [IR.MOD] = function(l, a, b)
        if l == IR.MOD and b % a == 0 then
            return true, b
        end
        return false
    end
}

local function inst(ir, irc)
    if fold[ir[1]] then
        -- TODO: check operands are numbers
        local l = ir[2]
        local r = ir[3]
        if l[1] == IR.CONST and r[1] == IR.CONST then
            -- e.g. 1 + 2 becomes 3
            return { IR.CONST, irc:constant(fold[ir[1]](irc.constants[l[2]], irc.constants[r[2]])) }
        elseif foldr[ir[1]] and fold[l[1]] and l[3][1] == IR.CONST and r[1] == IR.CONST then
            -- e.g. a + 1 + 2 gets parsed as (a + 1) + 2, properly becomes a + 3
            local should, result = foldr[ir[1]](l[1], irc.constants[l[3][2]], irc.constants[r[2]])
            if should then
                l[3] = irc:constant(result, true)
                return l
            else
                return ir
            end
        end
    end

    return ir
end

local function findconsts(ir)
    local consts = {}
    for i,v in ipairs(ir) do
        if i == 1 and v == IR.CONST then
            consts[#consts+1] = ir
        elseif type(v) == "table" then
            for _,v2 in ipairs(findconsts(v)) do
                consts[#consts+1] = v2
            end
        end
    end
    return consts
end

local function final(irc)
    -- remove unused (due to constant folding) constants
    local used, usedv, i = {}, {}, 0
    local insts = findconsts(irc.ir)
    for _,v in ipairs(insts) do
        if not used[v[2]] then
            used[v[2]] = i
            usedv[i] = irc.constants[v[2]]
            i = i + 1
        end
    end
    for _,v in ipairs(insts) do
        v[2] = used[v[2]]
    end

    local constvals = {}
    for k,v in pairs(usedv) do
        constvals[v] = k
    end
    irc.constants = usedv
    irc.constvals = constvals
end

return { inst=inst, final=final }

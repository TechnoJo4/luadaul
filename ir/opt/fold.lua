local IR = require("ir/insts")

-- typecheck
local function tc(a, b, at, bt)
    return type(a) == at and type(b) == bt
end

local N = "number"

-- "1 + 2" -> 3
local fold = {
    [IR.ADD] = function(a, b)
        if not tc(a, b, N, N) then
            return false
        end
        return true, a + b
    end,
    [IR.SUB] = function(a, b)
        if not tc(a, b, N, N) then
            return false
        end
        return true, a - b
    end,
    [IR.MUL] = function(a, b)
        if not tc(a, b, N, N) then
            return false
        end
        return true, a * b
    end,
    [IR.DIV] = function(a, b)
        if not tc(a, b, N, N) or a == 0 or b == 0 then
            return false
        end
        return true, a / b
    end,
    [IR.MOD] = function(a, b)
        if not tc(a, b, N, N) then
            return false
        end
        return true, a % b
    end,
    [IR.POW] = function(a, b)
        if not tc(a, b, N, N) then
            return false
        end
        return true, a ^ b
    end,
}

-- "a + 1 + 2", parsed as ((a + 1) + 2) -> a + 3
local foldr = {
    [IR.ADD] = function(l, a, b)
        if (l == IR.ADD or l == IR.SUB) and tc(a, b, N, N) then
            return true, fold[l](a, b)
        end
        return false
    end,
    [IR.MUL] = function(l, a, b)
        if (l == IR.MUL or l == IR.DIV) and tc(a, b, N, N) then
            return true, fold[l](a, b)
        end
        return false
    end,
    [IR.MOD] = function(l, a, b)
        if l == IR.MOD and b % a == 0 and tc(a, b, N, N) then
            return true, b
        end
        return false
    end,
    [IR.SUB] = function(l, a, b)
        if not tc(a, b, N, N) then
            return false
        elseif l == IR.ADD then
            return true, a - b
        elseif l == IR.SUB then
            return true, a + b
        end
        return false
    end,
    [IR.DIV] = function(l, a, b)
        if not tc(a, b, N, N) then
            return false
        elseif l == IR.MUL then
            return true, a / b
        elseif l == IR.DIV then
            return true, a * b
        end
        return false
    end
}

return function(ir, irc)
    if fold[ir[1]] then
        local l = ir[2]
        local r = ir[3]

        if l[1] == IR.CONST and r[1] == IR.CONST then
            local should, v = fold[ir[1]](irc.constants[l[2]], irc.constants[r[2]])

            if should then
                return { IR.CONST, irc:constant(v) }
            else
                return ir
            end
        elseif foldr[ir[1]] and fold[l[1]] and l[3][1] == IR.CONST and r[1] == IR.CONST then
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

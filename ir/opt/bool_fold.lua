local IR = require("ir/insts")

-- typecheck
local function tc(a, b, at, bt)
    return type(a) == at and type(b) == bt
end

local N = "number"

local insts = {
    [IR.LT] = true,
    [IR.GT] = true,
    [IR.LTEQ] = true,
    [IR.GTEQ] = true,
    [IR.EQ] = true,
    [IR.NEQ] = true,
    [IR.OR] = true,
    [IR.AND] = true
}

local cmp = {
    [IR.LT] = function(a, b)
        return a < b
    end,
    [IR.GT] = function(a, b)
        return a > b
    end,
    [IR.LTEQ] = function(a, b)
        return a <= b
    end,
    [IR.GTEQ] = function(a, b)
        return a >= b
    end
}

local eq = {
    [IR.EQ] = function(a, b)
        return a == b
    end,
    [IR.NEQ] = function(a, b)
        return a ~= b
    end
}

local pri_insts = {
    [IR.TRUE] = true,
    [IR.FALSE] = true,
    [IR.NIL] = true
}

return function(ir, irc)
    if ir[1] == IR.NOT then
        local r = ir[2]

        if r[1] == IR.CONST or r[1] == IR.TRUE then
            return { IR.FALSE }
        end

        if r[1] == IR.FALSE or r[1] == IR.NIL then
            return { IR.TRUE }
        end
    elseif insts[ir[1]] then
        local l = ir[2]
        local r = ir[3]

        if l[1] == IR.CONST then
            if ir[1] == IR.AND then
                return r
            end

            if ir[1] == IR.OR then
                return l
            end

            if r[1] == IR.CONST then
                local lhs, rhs = irc.constants[l[2]], irc.constants[r[2]]

                if cmp[ir[1]] and tc(lhs, rhs, N, N) then
                    if cmp[ir[1]](lhs, rhs) then
                        return { IR.TRUE }
                    else
                        return { IR.FALSE }
                    end
                elseif eq[ir[1]] then
                    if eq[ir[1]](lhs, rhs) then
                        return { IR.TRUE }
                    else
                        return { IR.FALSE }
                    end
                end
            end
        elseif pri_insts[l[1]] then
            if ir[1] == IR.AND then
                return l[1] == IR.TRUE and r or l
            end

            if ir[1] == IR.OR then
                return l[1] == IR.TRUE and l or r
            end
        end
    end

    return ir
end

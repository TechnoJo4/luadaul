local IR = require("ir.insts")
local PUSH = setmetatable({}, { __tostring=function()
    return "[PUSH]"
end })

local function same(a, b)
    local t = a[1]
    if b[1] ~= t then return false end

    if t == IR.CONST then
        return a[2] == b[2]
    elseif t == IR.GETLOCAL then
        return a[2] == b[2]
    end

    return false
end

local base = {}

base[IR.PUSH] = function(self, ir)
    local bc = {}
    for i=2,#ir do
        if type(ir[i]) == "number" then
            self:reg(ir[i], PUSH)
            bc[i-1] = ""
        else
            local a, ra = self:compile(ir[i])
            self:reg(ra, PUSH)
            bc[i-1] = a
        end
    end
    return table.concat(bc, "")
end

base[IR.POP] = function(self, ir)
    local bc = {}
    for i=2,#ir do
        if type(ir[i]) == "number" then
            self:reg(ir[i], false)
            bc[i-1] = ""
        else
            -- pushes are important allocations (i.e. locals),
            -- they shouldn't be overriden with expressions (e.g. assignments),
            -- which compile to IR.POP
            local a, ra = self:compile(ir[i])
            if self.regs[ra] ~= PUSH then
                self:reg(ra, false)
            end
            bc[i-1] = a
        end
    end
    return table.concat(bc, "")
end

return { same=same, base=base, PUSH=PUSH }

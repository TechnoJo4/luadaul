local IR = require("ir/insts")

local arr_insts = {
    [IR.LOOP] = { [2] = true },
    [IR.IF] = { [3] = true, [4] = true },
}

local c1_insts = {
    [IR.CONST] = true,
    [IR.GETGLOBAL] = true,
    [IR.SETGLOBAL] = true
}

local c2_insts = {
    [IR.NAMECALL] = true,
}

local function findconsts(ir, is_arr, insts_1, insts_2)
    for i,v in ipairs(ir) do
        local is_inst = i == 1 and not is_arr

        if is_inst and c1_insts[v] then
            insts_1[#insts_1+1] = ir
        elseif is_inst and c2_insts[v] then
            insts_2[#insts_2+1] = ir
        elseif type(v) == "table" then
            local arr = arr_insts[ir[1]]
            if arr then
                arr = arr[i]
            end

            findconsts(v, arr, insts_1, insts_2)
        end
    end

    return insts_1, insts_2
end

return function(irc)
    local used, usedv, i = {}, {}, 0
    local insts1, insts2 = findconsts(irc.ir, true, {}, {})

    -- add used constants to used array
    for _,v in ipairs(insts1) do
        if not used[v[2]] then
            used[v[2]] = i
            usedv[i] = irc.constants[v[2]]
            i = i + 1
        end
    end
    for _,v in ipairs(insts2) do
        if not used[v[3]] then
            used[v[3]] = i
            usedv[i] = irc.constants[v[3]]
            i = i + 1
        end
    end

    -- reassign constants in the instructions that use them
    for _,v in ipairs(insts1) do
        v[2] = used[v[2]]
    end
    for _,v in ipairs(insts2) do
        v[3] = used[v[3]]
    end

    -- reassign constants in IR compiler
    local constvals = {}
    for k,v in pairs(usedv) do
        constvals[v] = k
    end

    irc.constants = usedv
    irc.constvals = constvals
end

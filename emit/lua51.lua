--# selene: allow(bad_string_escape)
-- spaghetti

local bit = require("bit")
local ffi = require("ffi")
local IR = require("ir.insts")
local base = require("emit.base")
local same = base.same

local PUSH = base.PUSH

local u8 = string.char
local function i32(num)
    return u8(bit.band(num, 0xff))
        .. u8(bit.band(bit.rshift(num, 8), 0xff))
        .. u8(bit.band(bit.rshift(num, 16), 0xff))
        .. u8(bit.band(bit.rshift(num, 24), 0xff))
end
local function u64(num)
    return i32(num).."\0\0\0\0"
end

-- TODO: support non-luajit?
local double_t = ffi.typeof('double[1]')
local function f64(num)
    return ffi.string(double_t({ num }), 8)
end

local inst_s = i32

local size_C = 9
local size_A = 8

local size_OP = 6
local mask_OP = bit.lshift(1, size_OP)-1

local pos_OP = 0
local pos_A = pos_OP + size_OP
local pos_C = pos_A + size_A
local pos_B = pos_C + size_C
local pos_Bx = pos_C

local function iABC(o, a, b, c)
    return bit.bor(bit.lshift(o, pos_OP), bit.lshift(a or 0, pos_A), bit.lshift(b or 0, pos_B), bit.lshift(c or 0, pos_C))
end

local function iABx(o, a, b)
    return bit.bor(bit.lshift(o, pos_OP), bit.lshift(a or 0, pos_A), bit.lshift(b or 0, pos_Bx))
end

local opcodes = {
    [0] = "MOVE", -- A B     R(A) := R(B)
    [1] = "LOADK", -- A Bx    R(A) := Kst(Bx)
    [2] = "LOADBOOL", -- A B C   R(A) := (Bool)B; if (C) pc++
    [3] = "LOADNIL", -- A B     R(A) := ... := R(B) := nil
    [4] = "GETUPVAL", -- A B     R(A) := UpValue[B]
    [5] = "GETGLOBAL", -- A Bx    R(A) := Gbl[Kst(Bx)]
    [6] = "GETTABLE", -- A B C   R(A) := R(B)[RK(C)]
    [7] = "SETGLOBAL", -- A Bx    Gbl[Kst(Bx)] := R(A)
    [8] = "SETUPVAL", -- A B     UpValue[B] := R(A)
    [9] = "SETTABLE", -- A B C   R(A)[RK(B)] := RK(C)
    [10] = "NEWTABLE", -- A B C   R(A) := {} (size = B,C)
    [11] = "SELF", -- A B C   R(A+1) := R(B); R(A) := R(B)[RK(C)]
    [12] = "ADD", -- A B C   R(A) := RK(B) + RK(C)
    [13] = "SUB", -- A B C   R(A) := RK(B) - RK(C)
    [14] = "MUL", -- A B C   R(A) := RK(B) * RK(C)
    [15] = "DIV", -- A B C   R(A) := RK(B) / RK(C)
    [16] = "MOD", -- A B C   R(A) := RK(B) % RK(C)
    [17] = "POW", -- A B C   R(A) := RK(B) ^ RK(C)
    [18] = "UNM", -- A B     R(A) := -R(B)
    [19] = "NOT", -- A B     R(A) := not R(B)
    [20] = "LEN", -- A B     R(A) := length of R(B)
    [21] = "CONCAT", -- A B C   R(A) := R(B).. ... ..R(C)
    [22] = "JMP", -- sBx     pc+=sBx
    [23] = "EQ", -- A B C   if ((RK(B) == RK(C)) ~= A) then pc++
    [24] = "LT", -- A B C   if ((RK(B) <  RK(C)) ~= A) then pc++
    [25] = "LE", -- A B C   if ((RK(B) <= RK(C)) ~= A) then pc++
    [26] = "TEST", -- A C     if not (R(A) <=> C) then pc++
    [27] = "TESTSET", -- A B C   if (R(B) <=> C) then R(A) := R(B) else pc++
    [28] = "CALL", -- A B C   R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
    [29] = "TAILCALL", -- A B C   return R(A)(R(A+1), ... ,R(A+B-1))
    [30] = "RETURN", -- A B     return R(A), ... ,R(A+B-2)      (see note)
    [31] = "FORLOOP", -- A sBx   R(A)+=R(A+2); if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }
    [32] = "FORPREP", -- A sBx   R(A)-=R(A+2); pc+=sBx
    [33] = "TFORLOOP", -- A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2)); if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
    [34] = "SETLIST", -- A B C   R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
    [35] = "CLOSE", -- A       close all variables in the stack up to (>=) R(A)
    [36] = "CLOSURE", -- A Bx    R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
    [37] = "VARARG" -- A B     R(A), R(A+1), ..., R(A+B-1) = vararg
    -- "63" (instruction 0xffffffff) is a placeholder made by break that gets replaced by JMP
}

--[[
    R(x) - register
    Kst(x) - constant (in constant table)
    RK(x) == if ISK(x) then Kst(INDEXK(x)) else R(x)

    Notes:
    (*) In OP_CALL, if (B == 0) then B = top. C is the number of returns - 1,
        and can be 0: OP_CALL then sets `top' to last_result+1, so
        next open instruction (OP_CALL, OP_RETURN, OP_SETLIST) may use `top'.

    (*) In OP_VARARG, if (B == 0) then use actual number of varargs and
        set top (like in OP_CALL with C == 0).

    (*) In OP_RETURN, if (B == 0) then return up to `top'

    (*) In OP_SETLIST, if (B == 0) then B = `top';
        if (C == 0) then next `instruction' is real C

    (*) For comparisons, A specifies what condition the test should accept
        (true or false).

    (*) All `skips' (pc++) assume that next instruction is a jump
]]

local function getop(inst)
    return bit.band(inst:byte(1), mask_OP)
end

local function is_jmp(inst)
    local o = getop(inst)
    return o == 22 -- JMP
        or o == 63 -- break placeholder; replaced with JMP
end

local ABx_ops = { ["LOADK"]=true, ["GETGLOBAL"]=true, ["SETGLOBAL"]=true, ["CLOSURE"]=true }
local AsBx_ops = { ["JMP"]=true, ["FORLOOP"]=true, ["FORPREP"]=true }

local function str_tohex(s)
    return s:gsub(".", function(c)
        return bit.tohex(string.byte(c)):sub(-2,-1)
    end)
end

-- warning: debug printing not in the same order as the bytecode
local DEBUG_INSTS = false
local o = {}
for k,v in pairs(opcodes) do
    local f = iABC
    if ABx_ops[v] then
        f = iABx
    elseif AsBx_ops[v] then
        f = function(op, a, b)
            return iABx(op, a, b + 131071)
        end
    end
    local padded = v..(" "):rep(10-#v)
    if DEBUG_INSTS then
        o[v] = function(...)
            local s = inst_s(f(k, ...))
            print(padded, str_tohex(s), ...)
            return s
        end
    else
        o[v] = function(...)
            return inst_s(f(k, ...))
        end
    end
end

local function str(s, len_f)
    if s then
        return len_f(#s+1)..s.."\0"
    else
        return len_f(0)
    end
end

local function list(tbl, size)
    size = size or #tbl
    return i32(size)..table.concat(tbl, "")
end

local DEBUG_IPLINES = true
local function chunk(data)
    if DEBUG_IPLINES then
        for i=0,data.ninsts do
            data.linedata[i] = i32(i)
        end
    end
    return str(data.name, data.x64 and u64 or i32)
        .. i32(data.startline)
        .. i32(data.endline)
        .. u8(data.nupvals)
        .. u8(data.nparams)
        .. u8(data.is_vararg)
        .. u8(data.maxstack)
        .. list(data.code, data.ninsts)
        .. list(data.constants)
        .. list(data.protos)
        .. list(data.linedata) -- linedata (debug, optional)
        .. list({}) -- locals (debug, optional)
        .. list(data.upvals) -- debug, optional
end

local compiler = setmetatable({ cond={} }, { __index=base.base })
local function new_compiler(irc, x64)
    local size_t = x64 and u64 or i32

    local self = setmetatable({
        name=nil, startline=0, endline=0, is_vararg=0,
        nupvals=irc.nupvals, nparams=irc.nparams, maxstack=0,
        ninsts=0, code={}, constants={}, protos={},
        upvals={}, linedata={},
        irc=irc, scopedepth=0, regs={}, x64=x64,
        local_offsets={},
    }, { __index=compiler })

    self.irc:finalize()

    for k,v in pairs(irc.protos) do
        self.protos[k] = new_compiler(v, x64):compile_chunk()
    end

    for k,v in pairs(irc.upvals) do
        self.upvals[k] = str(v.name, size_t)
    end

    for k,v in pairs(irc.constants) do
        local t = type(v)
        if t == "number" then
            self.constants[k+1] = "\x03"..f64(v)
        elseif t == "string" then
            self.constants[k+1] = "\x04"..str(v, size_t)
        else
            error("invalid constant type")
        end
    end
    return self
end

local function shouldpop(self, r)
    return r < 255 and self.regs[r] ~= PUSH
end

-- tobool: convert to boolean
-- invert: skip next if false (jump if true) instead of skip next if true
function compiler:compile_cond(ir, tobool, invert, ...)
    if tobool then
        invert = not invert
    end
    local t = ir[1]
    local code
    local func = self.cond[t]
    if not func then
        if self[t] then
            local r
            code, r = self[t](self, ir, r, ...)
            code = code..o.TEST(r, 0, invert and 1 or 0)
            if shouldpop(self, r) then
                self:reg(r, false)
            end
        else
            error(("No lua51 compile rule for %s"):format(ir[1])) -- TODO: error
        end
    else
        code = func(self, ir, invert, ...)
    end
    if tobool then
        if tobool == true then
            tobool = self:reg()
        end
        code = code..o.JMP(0,1)..o.LOADBOOL(tobool, 1, 1)..o.LOADBOOL(tobool, 0, 0)
    end
    return code, tobool
end

local DEBUG_IR = false
function compiler:compile(ir, r, ...)
    if DEBUG_IR then
        print(ir)
    end

    local t = ir[1]
    local func = self[t]
    if not func then
        if self.cond[t] then
            return self:compile_cond(ir, r or true, false, ...)
        else
            error(("No lua51 compile rule for %s"):format(ir[1])) -- TODO: error
        end
    end
    return func(self, ir, r, ...)
end

function compiler:compile_all(tbl, allow_breaks)
    local old = self.breaks
    self.breaks = allow_breaks or self.breaks
    local bc = {}
    for i,v in ipairs(tbl) do
        bc[i] = self:compile(v)
    end
    self.breaks = old
    return table.concat(bc, "")
end

local DEBUG_REGS = false
function compiler:reg(r, v)
    if v == nil then
        v = true
    end
    if r then
        if r > 255 then
            error("Cannot allocate register "..tostring(r))
        end
        if DEBUG_REGS then
            print("allocn", r, v)
        end
        self.regs[r] = v
        if self.maxstack <= r then
            self.maxstack = r + 1
        end
        return r
    end

    for i=0,255 do
        if not self.regs[i] then
            if DEBUG_REGS then
                print("alloc", i, self.regs[i], "->", v)
            end
            self.regs[i] = v
            if self.maxstack <= i then
                self.maxstack = i + 1
            end
            return i
        end
    end
    error("Could not allocate register")
end

function compiler:RK(ir, r, ...)
    if r then
        return self:compile(ir, r, ...)
    end

    if type(ir) == "number" then
        return "", bit.bor(256, ir)
    elseif ir[1] == IR.CONST then
        return "", bit.bor(256, ir[2])
    elseif ir[1] == IR.GETLOCAL then
        return "", self:localreg(ir[2])
    else
        return self:compile(ir, r, ...)
    end
end

compiler[IR.CLOSE] = function(self, ir)
    local min = 256
    for i=2,#ir do
        local r = ir[i]
        self:reg(r, false)
        if min > r then
            min = r
        end
    end
    return o.CLOSE(min)
end

compiler[IR.RETURN] = function(self, v)
    local a, ra = self:compile(v[2])
    if shouldpop(self, ra) then
        self:reg(ra, false)
    end
    return a..o.RETURN(ra, 2)
end

compiler[IR.IF] = function(self, v)
    local t = self:compile_all(v[3])
    local jmp = is_jmp(t)
    local cond = self:compile_cond(v[2], false, jmp)
    local len = #t / 4
    if v[4] then
        local f = self:compile_all(v[4])
        t = t..o.JMP(0, #f / 4)..f
        len = len + 1
    end
    if not jmp then
        cond = cond..o.JMP(0, len)
    end
    return cond..t
end

compiler[IR.CONDITIONAL] = function(self, v, r)
    r = r or self:reg()
    local t = self:compile(v[3], r)
    local f = self:compile(v[4], r)
    return self:compile_cond(v[2])
        .. o.JMP(0, #t / 4 + 1) .. t
        .. o.JMP(0, #f / 4) .. f, r
end

compiler[IR.LJ_LOOP] = function(_self, _v)
    -- nothing. this instruction is only used on luajit
end

compiler[IR.LOOP] = function(self, v)
    local code = self:compile_all(v[2], true)
    local last = code:sub(-4, -1)
    if getop(last) == 35 then -- CLOSE
        code = code:sub(1, -5)
    else
        last = ""
    end
    local len = #code / 4
    code = code..o.JMP(0, -len - 1)
    code = code:gsub("()\xFF\xFF\xFF\xFF", function(pos)
        return o.JMP(0, len - pos/4)
    end)
    return code..last
end

compiler[IR.NUMFOR] = function(self, v)
    local r0 = self:reg()
    local prep = self:compile(v[2], r0)
            .. self:compile(v[3], self:reg(r0+1))
            .. self:compile(v[4], self:reg(r0+2))

    self:reg(r0+3, PUSH)
    local off = self.local_offsets
    off[#off+1] = { r0, 3 }

    local code = self:compile_all(v[5], true)
    local len = #code / 4
    code = prep..o.FORPREP(r0, len)..code..o.FORLOOP(r0, -len - 1)
    code = code:gsub("()\xFF\xFF\xFF\xFF", function(pos)
        return o.JMP(0, len - pos/4)
    end)

    self:reg(r0, false)
    self:reg(r0+1, false)
    self:reg(r0+2, false)
    self:reg(r0+3, false)
    off[#off] = nil
    return code
end

compiler[IR.ITERFOR] = function(self, v)
    local r0 = self:reg()
    local prep = self:compile(v[2], r0)

    for i=1,v[3] do
        self:reg(r0+2+i, PUSH)
    end
    local off = self.local_offsets
    off[#off+1] = { r0, 3 }

    local code = self:compile_all(v[4], true)
    local len = #code / 4
    code = prep..o.JMP(0, len)..code..o.TFORLOOP(r0, 0, v[3])..o.JMP(0, -(2 + len))
    code = code:gsub("()\xFF\xFF\xFF\xFF", function(pos)
        return o.JMP(0, len - pos/4 + 1)
    end)

    for i=0,v[3]+2 do
        self:reg(r0+i, false)
    end
    off[#off] = nil
    return code
end

compiler[IR.BREAK] = function(self)
    if not self.breaks then
        error("break not allowed outside loops") -- TODO: error
    end
    return "\xFF\xFF\xFF\xFF"
end

local function binop(inst)
    return function(self, v, r)
        local lhs, rhs = v[2], v[3]
        local b, rb
        local a, ra = self:RK(lhs, r)
        r = r or ra

        if same(lhs, rhs) then
            b, rb = "", ra
        else
            b, rb = self:RK(rhs)
        end

        local s = a..b..inst(r, ra, rb)

        if ra ~= rb and shouldpop(self, rb) then
            self:reg(rb, false)
        end
        if r ~= ra and shouldpop(self, ra) then
            self:reg(ra, false)
        end
        return s, r
    end
end
local function unop(inst)
    return function(self, v, ra)
        local a
        a, ra = self:compile(v[2], ra)
        local s = a..inst(ra, ra)
        return s, ra
    end
end

compiler[IR.FALSE] = function(self, _v, r)
    r = r or self:reg()
    return o.LOADBOOL(r, 0, 0), r
end

compiler[IR.TRUE] = function(self, _v, r)
    r = r or self:reg()
    return o.LOADBOOL(r, 1, 0), r
end

compiler[IR.NIL] = function(self, _v, r)
    r = r or self:reg()
    return o.LOADNIL(r, r), r
end

compiler[IR.CONST] = function(self, v, r)
    r = r or self:reg()
    return o.LOADK(r, v[2]), r
end

local _closure_upval_ops = { [IR.GETUPVAL] = o.GETUPVAL, [IR.GETLOCAL] = o.MOVE }
compiler[IR.CLOSURE] = function(self, v, r)
    r = r or self:reg()
    local bc = { o.CLOSURE(r, v[2]) }
    if #v > 2 then
        for i=3,#v do
            local ut, ui = unpack(v[i])
            bc[i-1] = _closure_upval_ops[ut](0, ui)
        end
    end
    return table.concat(bc, ""), r
end

compiler[IR.ADD] = binop(o.ADD)
compiler[IR.SUB] = binop(o.SUB)
compiler[IR.MUL] = binop(o.MUL)
compiler[IR.DIV] = binop(o.DIV)
compiler[IR.MOD] = binop(o.MOD)
compiler[IR.POW] = binop(o.POW)
compiler[IR.CONCAT] = function(self, v, r)
    local move = false
    if r then
        for i=2,#v do
            if self.regs[r+i-2] then
                move = r
                r = self:reg()
                break
            end
        end
    else
        r = self:reg()
    end

    local bc = {}
    local b = r
    local c = r
    for i=2,#v do
        local code, reg = self:compile(v[i], self:reg(r + i - 2))
        bc[i-1] = code
        c = reg
    end

    local s = table.concat(bc, "")..o.CONCAT(move or r, b, c)
    for i=move and b+1 or b,c do
        self.regs[i] = false
    end
    return s, r
end

compiler[IR.UNM] = unop(o.UNM)
compiler[IR.LEN] = unop(o.LEN)
compiler.cond[IR.NOT] = function(self, v, invert)
    return self:compile_cond(v[2], false, not invert)
end

local function bincmp(inst, swap)
    return function(self, v, invert)
        local lhs, rhs = v[2], v[3]

        local b, rc
        local a, rb = self:RK(lhs)
        if same(lhs, rhs) then
            b, rc = "", rb
        else
            b, rc = self:RK(rhs)
        end

        if swap then
            rc, rb = rb, rc
        end
        local s = a..b..inst(invert and 1 or 0, rb, rc)

        if shouldpop(self, rc) then
            self:reg(rc, false)
        end
        if shouldpop(self, rb) then
            self:reg(rb, false)
        end
        return s
    end
end

compiler.cond[IR.EQ] = bincmp(o.EQ)
compiler.cond[IR.NEQ] = bincmp(o.EQ, true)
compiler.cond[IR.LT] = bincmp(o.LT)
compiler.cond[IR.LTEQ] = bincmp(o.LE)
compiler.cond[IR.GT] = bincmp(o.LT, true)
compiler.cond[IR.GTEQ] = bincmp(o.LE, true)

function compiler:localreg(idx)
    for i=1,#self.local_offsets do
        local off = self.local_offsets[i]
        if idx >= off[1] then
            idx = idx + off[2]
        end
    end
    return idx
end

local function andor(c)
    return function(self, v, ra)
        ra = ra or self:reg()

        local lhs, rhs = v[2], v[3]

        -- optimize x = x or y
        if lhs[1] == IR.GETLOCAL and ra == self:localreg(lhs[2]) then
            local a = self:compile(rhs, ra)
            return o.TEST(ra, 0, c)
                .. o.JMP(0, #a / 4)
                .. a, ra
        end

        local rb = self:reg()
        local b = self:compile(lhs, rb)
        self:reg(rb, false)
        local a = self:compile(rhs, ra)

        return b
            .. o.TESTSET(ra, rb, c)
            .. o.JMP(0, #a / 4)
            .. a, ra
    end
end

local function andor_cond(c)
    return function(self, v, invert)        
        local lhs, rhs = v[2], v[3]
        lhs = self:compile_cond(lhs, false, c)
        rhs = self:compile_cond(rhs, false, invert)
        return lhs..o.JMP(0, #rhs / 4)..rhs
    end
end

compiler[IR.AND] = andor(0)
compiler[IR.OR] = andor(1)
compiler.cond[IR.AND] = andor_cond(false)
compiler.cond[IR.OR] = andor_cond(true)


compiler[IR.GETGLOBAL] = function(self, v, a)
    a = a or self:reg()
    return o.GETGLOBAL(a, v[2]), a
end

compiler[IR.SETGLOBAL] = function(self, v, a)
    local code
    code, a = self:compile(v[3], a)
    return code..o.SETGLOBAL(a, v[2]), a
end

compiler[IR.GETLOCAL] = function(self, v, r)
    local l = self:localreg(v[2])
    if not r or r == l then
        return "", l
    else
        return o.MOVE(r, l), r
    end
end

compiler[IR.GETUPVAL] = function(self, v, r)
    r = r or self:reg()
    return o.GETUPVAL(r, v[2]), r
end

compiler[IR.SETLOCAL] = function(self, v, r)
    local n = self:localreg(v[2])
    if not r or r == n then
        return self:compile(v[3], n)
    else
        local code = self:compile(v[3], r)
        return code..o.MOVE(n, r), r
    end
end

compiler[IR.SETUPVAL] = function(self, v, r)
    local a
    a, r = self:compile(v[3], r)
    return a..o.SETUPVAL(r, v[2]), r
end

compiler[IR.GETTABLE] = function(self, v, ra)
    local b, rb = self:compile(v[2])
    local c, rc = self:RK(v[3])
    ra = ra or rb
    return b..c..o.GETTABLE(ra, rb, rc), ra
end

compiler[IR.SETTABLE] = function(self, v, r)
    local a, ra = self:compile(v[2])
    local b, rb = self:RK(v[3])
    local c, rc = self:RK(v[4], r)
    return c..a..b..o.SETTABLE(ra, rb, rc), rc
end

compiler[IR.CALL] = function(self, v, a)
    a = a or self:reg()
    local target
    target, a = self:compile(v[2], a)
    local b, nargs = 1, 0
    local bc = { target }
    if #v > 3 then
        for i=4,#v do
            local code = self:compile(v[i], self:reg(a + i - 3))
            bc[i - 2] = code
            b = i - 2
            nargs = nargs + 1
        end
    end
    local nrets = v[3]
    local max = math.max(nargs+1, nrets)
    for i=0,max-1 do
        self.regs[a+i] = i < nrets
    end
    return table.concat(bc, "")..o.CALL(a, b, v[3]+1), a
end

compiler[IR.NAMECALL] = function(self, v, a)
    local from, b = self:compile(v[2], a)
    local target = o.SELF(b, b, bit.bor(256, v[3]))
    self:reg(b + 1)
    local nargs
    b, nargs = 2, 1
    local bc = { from, target }
    if #v > 4 then
        for i=5,#v do
            local code = self:compile(v[i], self:reg(a + i - 3))
            bc[i - 2] = code
            b = i - 2
            nargs = nargs + 1
        end
    end
    local nrets = v[4]
    local max = math.max(nargs+1, nrets)
    for i=0,max-1 do
        self.regs[a+i] = i < nrets
    end
    return table.concat(bc, "")..o.CALL(a, b, v[4]+1), a
end

function compiler:compile_chunk()
    for i,v in ipairs(self.irc.ir) do
        local code = self:compile(v)
        self.code[i] = code
        self.ninsts = self.ninsts + #code / 4
    end
    self.code[#self.code+1] = o.RETURN(0, 1)
    self.ninsts = self.ninsts + 1

    if DEBUG_REGS then
        print("Regs:")
        for i=0,#self.regs do
            print("", i, self.regs[i])
        end
    end

    return chunk(self)
end

function compiler:compile_main(name)
    self.name = name
    return "\x1BLua\x51\x00\x01\x04"
        .. (self.x64 and "\x08" or "\x04")
        .. "\x04\x08\x00"
        .. self:compile_chunk()
end

return { compiler=compiler, new_compiler=new_compiler }

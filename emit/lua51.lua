-- spaghetti

local bit = require("bit")
local ffi = require("ffi")
local irc = require("emit.ir")
local IR = irc.IR

-- TODO: support non-luajit?
-- i should probably find a safer way to do this anyways
local function i8(num)
    local ptr = ffi.new("uint8_t[1]", { num })
    return ffi.string(ptr, 1)
end
local function i32(num)
    local ptr = ffi.new("uint32_t[1]", { num })
    return ffi.string(ptr, 4)
end
local function i64(num)
    local ptr = ffi.new("uint64_t[1]", { num })
    return ffi.string(ptr, 8)
end
local function f64(num)
    local ptr = ffi.new("double[1]", { num })
    return ffi.string(ptr, 8)
end

local inst_s = i32

local size_C = 9
local size_B = 9
local size_Bx = size_C + size_B
local size_A = 8

local size_OP = 6

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
    "LOADK", -- A Bx    R(A) := Kst(Bx)
    "LOADBOOL", -- A B C   R(A) := (Bool)B; if (C) pc++
    "LOADNIL", -- A B     R(A) := ... := R(B) := nil
    "GETUPVAL", -- A B     R(A) := UpValue[B]
    "GETGLOBAL", -- A Bx    R(A) := Gbl[Kst(Bx)]
    "GETTABLE", -- A B C   R(A) := R(B)[RK(C)]
    "SETGLOBAL", -- A Bx    Gbl[Kst(Bx)] := R(A)
    "SETUPVAL", -- A B     UpValue[B] := R(A)
    "SETTABLE", -- A B C   R(A)[RK(B)] := RK(C)
    "NEWTABLE", -- A B C   R(A) := {} (size = B,C)
    "SELF", -- A B C   R(A+1) := R(B); R(A) := R(B)[RK(C)]
    "ADD", -- A B C   R(A) := RK(B) + RK(C)
    "SUB", -- A B C   R(A) := RK(B) - RK(C)
    "MUL", -- A B C   R(A) := RK(B) * RK(C)
    "DIV", -- A B C   R(A) := RK(B) / RK(C)
    "MOD", -- A B C   R(A) := RK(B) % RK(C)
    "POW", -- A B C   R(A) := RK(B) ^ RK(C)
    "UNM", -- A B     R(A) := -R(B)
    "NOT", -- A B     R(A) := not R(B)
    "LEN", -- A B     R(A) := length of R(B)
    "CONCAT", -- A B C   R(A) := R(B).. ... ..R(C)
    "JMP", -- sBx     pc+=sBx
    "EQ", -- A B C   if ((RK(B) == RK(C)) ~= A) then pc++
    "LT", -- A B C   if ((RK(B) <  RK(C)) ~= A) then pc++
    "LE", -- A B C   if ((RK(B) <= RK(C)) ~= A) then pc++
    "TEST", -- A C     if not (R(A) <=> C) then pc++
    "TESTSET", -- A B C   if (R(B) <=> C) then R(A) := R(B) else pc++
    "CALL", -- A B C   R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
    "TAILCALL", -- A B C   return R(A)(R(A+1), ... ,R(A+B-1))
    "RETURN", -- A B     return R(A), ... ,R(A+B-2)      (see note)
    "FORLOOP", -- A sBx   R(A)+=R(A+2); if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }
    "FORPREP", -- A sBx   R(A)-=R(A+2); pc+=sBx
    "TFORLOOP", -- A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2)); if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
    "SETLIST", -- A B C   R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
    "CLOSE", -- A       close all variables in the stack up to (>=) R(A)
    "CLOSURE", -- A Bx    R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
    "VARARG" -- A B     R(A), R(A+1), ..., R(A+B-1) = vararg
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

local ABx_ops = { ["LOADK"]=true, ["GETGLOBAL"]=true, ["SETGLOBAL"]=true, ["CLOSURE"]=true }
local AsBx_ops = { ["JMP"]=true, ["FORLOOP"]=true, ["FORPREP"]=true }

local o = {}
for k,v in pairs(opcodes) do
    if ABx_ops[v] or AsBx_ops[v] then
        o[v] = function(...)
            return inst_s(iABx(k, ...))
        end
    else
        o[v] = function(...)
            return inst_s(iABC(k, ...))
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

-- name, startline, endline, nupvals, nparams, is_vararg, maxstack,
-- code, constants, protos, <linedata, locals, upvalues>(debug, optional)
local function chunk(data)
    return str(data.name, data.x64 and i64 or i32)
        .. i32(data.startline)
        .. i32(data.endline)
        .. i8(data.nupvals)
        .. i8(data.nparams)
        .. i8(data.is_vararg)
        .. i8(data.maxstack)
        .. list(data.code, data.ninsts)
        .. list(data.constants)
        .. list(data.protos)
        .. list({}) -- linedata
        .. list({}) -- locals
        .. list({}) -- upvalues
end

local compiler = {}
local function new_compiler(irc, x64)
    local size_t = x64 and i64 or i32

    local self = setmetatable({
        name=nil, startline=0, endline=0, is_vararg=0,
        nupvals=0, nparams=0, maxstack=0,
        ninsts=0, code={}, constants={}, protos={},
        irc=irc, scopedepth=0, regs={}, x64=x64
    }, { __index=compiler })

    for k,v in pairs(irc.constants) do
        local t = type(k)
        if t == "number" then
            self.constants[v+1] = "\x03"..f64(k)
        elseif t == "string" then
            self.constants[v+1] = "\x04"..str(k, size_t)
        else
            error()
        end
    end
    return self
end

function compiler:compile(ir, ...)
    local func = self[ir[1]]
    if not func then
        error(("No lua51 compile rule for %s"):format(ir[1])) -- TODO: error
    end
    return func(self, ir, ...)
end

function compiler:reg(n)
    if n then
        print("allocn",n)
        self.regs[n] = true
        if self.maxstack <= n then
            self.maxstack = n+1
        end
        return n
    end
    for i=self.irc.nlocals[self.scopedepth],255 do
        if not self.regs[i] then
            self.regs[i] = true
            if self.maxstack <= i then
                self.maxstack = i+1
            end
            print("alloc",i)
            return i
        end
    end
    error("Could not allocate register")
end

function compiler:RK(ir)
    if type(ir) == "number" then
        return "", bit.bor(256, ir)
    elseif ir[1] == IR.CONST then
        return "", bit.bor(256, ir[2])
    else
        return self:compile(ir)
    end
end

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

local function binop(inst)
    return function(self, v, r)
        local lhs, rhs = v[2], v[3]

        local b, rb
        local a, ra = self:compile(lhs, r)
        if same(lhs, rhs) then
            b, rb = "", ra
        else
            b, rb = self:compile(rhs)
        end

        local s = a..b..inst(ra, ra, rb)

        if ra ~= rb then
            self.regs[rb] = false
        end
        return s, ra
    end
end
local function unop(inst)
    return function(self, v, ra)
        local a, ra = self:compile(v[2], ra)
        local s = a..inst(ra, ra)
        return s, ra
    end
end

compiler[IR.NIL] = function(self, v, r)
    r = r or self:reg()
    return o.LOADNIL(r, r)
end

compiler[IR.ADD] = binop(o.ADD)
compiler[IR.SUB] = binop(o.SUB)
compiler[IR.MUL] = binop(o.MUL)
compiler[IR.DIV] = binop(o.DIV)
compiler[IR.MOD] = binop(o.MOD)
compiler[IR.POW] = binop(o.POW)
compiler[IR.CONCAT] = function(self, v, r)
    local lhs, rhs = v[2], v[3]
    local move = false
    if r then
        for i=2,#v do
            if self.regs[r+i-2] then
                move = r
                r = self:reg()
            end
        end
    end

    local bc = {}
    local b = r
    local c = r
    for i=2,#v do
        local code, reg = self:compile(v[i], self:reg(r + i - 2))
        bc[i-1] = code
        c = reg
    end

    local s, r = table.concat(bc, "")..o.CONCAT(move or r, b, c)
    for i=move and b+1 or b,c do
        self.regs[i] = false
    end
    return s, r
end

compiler[IR.UNM] = unop(o.UNM)
compiler[IR.NOT] = unop(o.NOT)
compiler[IR.LEN] = unop(o.LEN)

compiler[IR.CONST] = function(self, v, a)
    a = a or self:reg()
    return o.LOADK(a, v[2]), a
end

compiler[IR.POP] = function(self, v)
    if type(v[2]) == "number" then
        self.regs[v[2]] = false
        return ""
    end

    local a, ra = self:compile(v[2])
    self.regs[ra] = false
    return a
end

compiler[IR.RETURN] = function(self, v)
    local a, ra = self:compile(v[2])
    return a..o.RETURN(ra, 2)
end

compiler[IR.GETGLOBAL] = function(self, v, a)
    a = a or self:reg()
    return o.GETGLOBAL(a, v[2]), a
end

compiler[IR.SETGLOBAL] = function(self, v, a)
    local code, a = self:compile(v[3], a)
    return code..o.SETGLOBAL(a, v[2]), a
end

compiler[IR.GETLOCAL] = function(self, v, r)
    if not r then
        return "", v[2]
    else
        return o.MOVE(r, v[2]), r
    end
end

compiler[IR.SETLOCAL] = function(self, v, r)
    if not r or r == v[2] then
        return self:compile(v[3], v[2])
    else
        local code, a = self:compile(v[3], r)
        return code..o.MOVE(v[2], r), r
    end
end

compiler[IR.GETTABLE] = function(self, v, ra)
    local b, rb = self:compile(v[2])
    local c, rc = self:RK(v[3])
    ra = ra or rb
    return b..c..o.GETTABLE(ra, rb, rc), ra
end

compiler[IR.CALL] = function(self, v, a)
    a = a or self:reg()
    local target, a = self:compile(v[2], a)
    local b = 1
    local bc = { target }
    if #v > 3 then
        for i=4,#v do
            local j = i-3
            local code, r = self:compile(v[i], self:reg(a + j))
            bc[j+1] = code
            b = j + 1
        end
    end
    return table.concat(bc, "")..o.CALL(a, b, v[3]+1), a
end

function compiler:compile_chunk()
    for i,v in ipairs(self.irc.ir) do
        local code, reg = self:compile(v)
        self.code[i] = code
        self.ninsts = self.ninsts + #code / 4
    end
    self.code[#self.code+1] = o.RETURN(0, 1)
    self.ninsts = self.ninsts + 1

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

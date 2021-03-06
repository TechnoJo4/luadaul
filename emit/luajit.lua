--# selene: allow(bad_string_escape)
-- spaghetti

local bit = require("bit")
local ffi = require("ffi")
local IR = require("ir/insts")
local base = require("emit/base")
local enum = require("common/utils").enum

local bor, band, rshift, lshift = bit.bor, bit.band, bit.rshift, bit.lshift

local PUSH = base.PUSH

local i16_t = ffi.typeof('int16_t[1]')
local double_t = ffi.typeof('double[1]')

local u8 = string.char
local function u16(num)
    return u8(band(num, 0xff))
        .. u8(band(rshift(num, 8), 0xff))
end
local function i16(num)
    return u16(num + 0x7fff)
end

local function f64_b(num)
    local v = double_t(num)
    local data = ffi.cast('int32_t*', v)
    return data[0], data[1]
end

--[==[
    bytecode dump format description from lj_bcdump.h

    dump   = header proto+ 0U
    header = ESC 'L' 'J' versionB flagsU [namelenU nameB*]
    proto  = lengthU pdata
    pdata  = phead bcinsW* uvdataH* kgc* knum* [debugB*]
    phead  = flagsB numparamsB framesizeB numuvB numkgcU numknU numbcU
             [debuglenU [firstlineU numlineU]]
    kgc    = kgctypeU { ktab | (loU hiU) | (rloU rhiU iloU ihiU) | strB* }
    knum   = intU0 | (loU1 hiU)
    ktab   = narrayU nhashU karray* khash*
    karray = ktabk
    khash  = ktabk ktabk
    ktabk  = ktabtypeU { intU | (loU hiU) | strB* }

    B = 8 bit, H = 16 bit, W = 32 bit, U = ULEB128 of W, U0/U1 = ULEB128 of W+1
]==]

local function uleb128(num)
    local bytes = {}
    while true do
        local byte = band(num, 0x7f)
        num = rshift(num, 7)
        if num == 0 then
            bytes[#bytes+1] = u8(byte)
            return table.concat(bytes, "") 
        end
        bytes[#bytes+1] = u8(bor(byte, 0x80))
    end
end

local function uleb128_33(num, b)
    local byte = bor(lshift(band(num, 0x3f), 1), b)

    num = rshift(num, 6)
    if num == 0 then
        return u8(byte)
    end

    local bytes = { u8(bor(byte, 0x80)) }
    while true do
        byte = band(num, 0x7f)
        num = rshift(num, 7)
        if num == 0 then
            bytes[#bytes+1] = u8(byte)
            return table.concat(bytes, "") 
        end
        bytes[#bytes+1] = u8(bor(byte, 0x80))
    end
end

local function is_int(num)
    return num == bor(0, num)
end

local function is_int16(num)
    return num == i16_t({ num })[0]
end

local function iABC(o, a, b, c)
    return u8(o, a or 0, c or 0, b or 0)
end

local function iAD(o, a, d)
    return u8(o, a or 0) .. u16(d or 0)
end

local function iAJ(o, a, d)
    return u8(o, a or 0) .. i16(d or 0)
end

-- http://wiki.luajit.org/Bytecode-2.0
local BC_ABC, BC_AD, BC_AJ = 0, 1, 2

--[[
    instruction name suffixes:
     - V: variable slot
     - S: string constant
     - N: number constant
     - P: primitive type
     - B: unsigned byte literal
     - M: multiple arguments/results
]]
local opcodes = enum({
    [0] = "ISLT", "ISGE", "ISLE", "ISGT",
    "ISEQV", "ISNEV",
    "ISEQS", "ISNES",
    "ISEQN", "ISNEN",
    "ISEQP", "ISNEP",
    "ISTC", "ISFC", "IST", "ISF", "ISTYPE", "ISNUM",
    "MOV",
    "NOT", "UNM", "LEN",
    "ADDVN", "SUBVN", "MULVN", "DIVVN", "MODVN",
    "ADDNV", "SUBNV", "MULNV", "DIVNV", "MODNV",
    "ADDVV", "SUBVV", "MULVV", "DIVVV", "MODVV",
    "POW", "CAT",
    "KSTR", "KCDATA", "KSHORT", "KNUM", "KPRI", "KNIL",
    "UGET",
    "USETV", "USETS", "USETN", "USETP",
    "UCLO", "FNEW",
    "TNEW", "TDUP",
    "GGET", "GSET",
    "TGETV", "TGETS", "TGETB", "TGETR",
    "TSETV", "TSETS", "TSETB", "TSETM", "TSETR",
    "CALLM", "CALL", "CALLMT", "CALLT",
    "ITERC", "ITERN",
    "VARG",
    "ISNEXT",
    "RETM", "RET", "RET0", "RET1",
    "FORI", "JFORI", "FORL",
    "IFORL","JFORL", "ITERL", "IITERL","JITERL",
    "LOOP", "ILOOP", "JLOOP",
    "JMP",
    -- functions headers (below) are not included in luajit bytecode dumps
    "FUNCF", "IFUNCF", "JFUNCF",
    "FUNCV", "IFUNCV", "JFUNCV",
    "FUNCC", "FUNCCW"
}, true, true)

local modes = {
    [0] = BC_AD, BC_AD, BC_AD, BC_AD,
    BC_AD, BC_AD,
    BC_AD, BC_AD,
    BC_AD, BC_AD,
    BC_AD, BC_AD,
    BC_AD, BC_AD, BC_AD, BC_AD, BC_AD, BC_AD,
    BC_AD,
    BC_AD, BC_AD, BC_AD,
    BC_ABC, BC_ABC, BC_ABC, BC_ABC, BC_ABC,
    BC_ABC, BC_ABC, BC_ABC, BC_ABC, BC_ABC,
    BC_ABC, BC_ABC, BC_ABC, BC_ABC, BC_ABC,
    BC_ABC, BC_ABC,
    BC_AD, BC_AD, BC_AD, BC_AD, BC_AD, BC_AD,
    BC_AD,
    BC_AD, BC_AD, BC_AD, BC_AD,
    BC_AJ, BC_AD,
    BC_AD, BC_AD,
    BC_AD, BC_AD,
    BC_ABC, BC_ABC, BC_ABC, BC_ABC,
    BC_ABC, BC_ABC, BC_ABC, BC_AD, BC_ABC,
    BC_ABC, BC_ABC, BC_AD, BC_AD,
    BC_ABC, BC_ABC,
    BC_ABC,
    BC_AJ,
    BC_AD, BC_AD, BC_AD, BC_AD,
    BC_AJ, BC_AJ, BC_AJ, BC_AJ, BC_AD,
    BC_AJ, BC_AJ, BC_AD,
    BC_AJ, BC_AJ, BC_AD, BC_AJ,
    BC_AD, BC_AD, BC_AD,
    BC_AD, BC_AD, BC_AD,
    BC_AD, BC_AD
}

local function str_tohex(s)
    local v = s:gsub(".", function(c)
        return bit.tohex(string.byte(c)):sub(-2,-1)
    end)
    return v
end

local DEBUG_INSTS = false
local o = {}
for k,v in pairs(opcodes) do
    if type(k) == "number" then
        local mode = modes[k]
        local padded = v..(" "):rep(10-#v)
        local f = iAD
        if mode == BC_ABC then
            f = iABC
        elseif mode == BC_AJ then
            f = iAJ
        end

        if DEBUG_INSTS then
            o[v] = function(...)
                local s = f(k, ...)
                print(padded, str_tohex(s), ...)
                return s
            end
        else
            o[v] = function(...)
                return f(k, ...)
            end
        end
    end
end

local function getop(inst)
    return inst:byte(1)
end

local function is_jmp(inst)
    local op = getop(inst)
    return op == o.JMP or op == 255 -- 0xFF is break placeholder and replaced with JMP
end

local VKNIL, VKFALSE, VKTRUE = 0, 1, 2

local KOBJ = enum({
    [0] = "CHILD", "TAB", "I64", "U64", "COMPLEX", "STR",
}, false, true)

-- selene: allow(unused_variable)
local KTAB = enum({
    [0] = "NIL", "FALSE", "TRUE", "INT", "NUM", "STR",
}, false, true)

-- see lj_bcwrite.c
local function chunk(data, _strip)
    --[=[
        proto  = lengthU pdata
        pdata  = phead bcinsW* uvdataH* kgc* knum* [debugB*]
        phead  = flagsB numparamsB framesizeB numuvB numkgcU numknU numbcU
                 [debuglenU [firstlineU numlineU]]
    ]=]

    local function reverse(tbl)
        local new = {}
        for i=1,#tbl do
            new[#tbl - i + 1] = tbl[i]
        end
        return new
    end

    local kobj = reverse(data.kobj)
    local knum = data.knum--reverse(data.knum)
    local pd = u8(data.flags) -- PROTO_CHILD | PROTO_VARARG | PROTO_FFI, see lj_obj.h
            .. u8(data.nparams)
            .. u8(data.maxstack)
            .. u8(#data.upvals)
            .. uleb128(#kobj) -- numkgc
            .. uleb128(#knum) -- numkn
            .. uleb128(data.ninsts) -- numbc
            .. table.concat(data.code)
            .. table.concat(data.upvals)
            .. table.concat(kobj)
            .. table.concat(knum)

    return table.concat(data.protos) .. uleb128(#pd) .. pd
end

-- IR -> bytecode compile rules
local compiler = {
    comp = setmetatable({}, { __index=base.base }), -- everything else
    cond = {} -- conditions
}

local function new_compiler(irc)
    local self = setmetatable({
        irc=irc,
        nparams=irc.nparams,
        maxstack=0, ninsts=0,
        protos={}, code={},
        kobj={}, knum={}, kshort={},
        const_i = {}, const_t = {},
        startline=0, endline=0, upvals={}, linedata={},
        scopedepth=0, regs={}, local_offsets={},
    }, { __index=compiler })

    self.irc:finalize()

    self.flags = 0x00
    for k,v in pairs(irc.protos) do
        self.flags = 0x01 -- PROTO_CHILD
        self.protos[k] = new_compiler(v):compile_chunk()
        self.kobj[#self.kobj+1] = uleb128(KOBJ.CHILD)
    end

    for k,v in ipairs(irc.upvals) do
        local i = v.i
        if not v.upval then
            if v.l.mutable then
                i = bor(i, 0x8000)
            else
                i = bor(i, 0xC000)
            end
        end

        self.upvals[k] = u16(i)
    end

    -- knum   = intU0 | (loU1 hiU)
    -- kgc    = kgctypeU { ktab | (loU hiU) | (rloU rhiU iloU ihiU) | strB* }
    self.constants = irc.constants
    for k,v in pairs(irc.constants) do
        local t = type(v)
        if t == "number" then
            if is_int16(v) then
                self.const_t[k] = opcodes.KSHORT
                self.kshort[k] = v

                -- this wastes a slot in constant table if the constant is never used in an
                -- operation that takes a constant index operand, but compiling 1 + v
                -- as KSHORT, ADDVV instead of just ADDVN is certainly more wasteful,
                -- and i'm really not willing to start analyzing the entire IR beforehand
                -- just to know if that's the case and save 2 bytes of the final bc dump
                self.const_i[k] = #self.knum
                self.knum[#self.knum+1] = uleb128_33(v, 0)
            elseif is_int(v) then
                self.const_t[k] = opcodes.KNUM
                self.const_i[k] = #self.knum
                self.knum[#self.knum+1] = uleb128_33(v, 0)
            else
                local lo, hi = f64_b(v)
                self.const_t[k] = opcodes.KNUM
                self.const_i[k] = #self.knum
                self.knum[#self.knum+1] = uleb128_33(lo, 1)..uleb128(hi)
            end
        elseif t == "string" then
            self.const_t[k] = opcodes.KSTR
            self.const_i[k] = #self.kobj
            self.kobj[#self.kobj+1] = uleb128(KOBJ.STR + #v)..v
        else
            error("invalid constant")
        end
    end

    return self
end

local function shouldpop(self, r)
    return r < 255 and self.regs[r] ~= PUSH
end

local DEBUG_IR = true
function compiler:compile(ir, r, ...)
    if DEBUG_IR then
        print(ir)
    end

    local t = ir[1]
    local func = self.comp[t]
    if not func then
        if self.cond[t] then
            return self:compile_cond(ir, r or true, false, ...)
        else
            error(("No luajit compile rule for %s"):format(ir[1])) -- TODO: error
        end
    end
    return func(self, ir, r, ...)
end

function compiler:compile_all(tbl, allow_breaks)
    local old = self.breaks
    if allow_breaks then 
        self.breaks = true
    end

    local bc = {}
    for i,v in ipairs(tbl) do
        bc[i] = self:compile(v)
    end

    self.breaks = old
    return table.concat(bc, "")
end

-- tobool: convert to boolean
-- invert: jump if true instead of jump if false
function compiler:compile_cond(ir, tobool, invert, ...)
    if tobool then
        invert = not invert
    end

    local t = ir[1]
    local code
    local func = self.cond[t]
    if not func then
        if self.comp[t] then
            local r
            code, r = self.comp[t](self, ir, r, ...)
            code = code..(invert and o.IST or o.ISF)(0, r)
            if shouldpop(self, r) then
                self:reg(r, false)
            end
        else
            error(("No luajit compile rule for %s"):format(ir[1])) -- TODO: error
        end
    else
        code = func(self, ir, invert, ...)
    end

    if tobool then
        local r = self:jmp_reg()
        if tobool == true then
            tobool = self:reg()
        end
        code = code..o.JMP(r, 2)..o.KPRI(tobool, 1)..o.JMP(r, 1)..o.KPRI(tobool, 2)
    end

    return code, tobool
end

local DEBUG_REGS = true
function compiler:jmp_reg()
    for i=0,255 do
        if not self.regs[i] then
            if DEBUG_REGS then
                print("jmp_reg", i)
            end
            return i
        end
    end
end

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
            self.maxstack = r+1
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
                self.maxstack = i+1
            end
            return i
        end
    end
    error("Could not allocate register")
end

compiler.comp[IR.FALSE] = function(self, _v, r)
    r = r or self:reg()
    return o.KPRI(r, VKFALSE), r
end

compiler.comp[IR.TRUE] = function(self, _v, r)
    r = r or self:reg()
    return o.KPRI(r, VKTRUE), r
end

compiler.comp[IR.NIL] = function(self, _v, r)
    r = r or self:reg()
    return o.KPRI(r, VKNIL), r
end

compiler.comp[IR.CONST] = function(self, v, r)
    r = r or self:reg()
    local op = self.const_t[v[2]]
    if op == opcodes.KSHORT then
        v = self.kshort[v[2]]
    else
        v = self.const_i[v[2]]
    end
    return o[opcodes[op]](r, v), r
end

-- NOTE: whenever emitting JMP, check for IR.CLOSE beforehand and inline jump instead
compiler.comp[IR.CLOSE] = function(self, ir)
    local min = 256
    for i=2,#ir do
        local r = ir[i]
        self:reg(r, false)
        if min > r then
            min = r
        end
    end
    return o.UCLO(min, 1)
end

compiler.comp[IR.CLOSURE] = function(self, v, r)
    r = r or self:reg()
    return o.FNEW(r, v[2]), r
end

-- check constant type
local function tconst(self, ir, t)
    if ir[1] ~= IR.CONST then return false end
    if not t then return true end

    local ct = self.const_t[ir[2]]
    if ct == opcodes.KSHORT then
        ct = opcodes.KNUM
    end

    return ct == t and self.const_i[ir[2]]
end

-- check primitive type
local function tpri(self, ir, t)
    return (ir[1] == IR.NIL and (not t or t == VKNIL) and VKNIL)
        or (ir[1] == IR.FALSE and (not t or t == VKFALSE) and VKFALSE)
        or (ir[1] == IR.TRUE and (not t or t == VKTRUE) and VKTRUE)
end

compiler.comp[IR.RETURN] = function(self, v)
    local a, ra = self:compile(v[2])
    if shouldpop(self, ra) then
        self:reg(ra, false)
    end

    local uclo = ""
    if v[3] then
        uclo = o.UCLO(v[3], 1)
    end

    -- TODO: multiple returns
    return a..uclo..o.RET1(ra, 2)
end

compiler.comp[IR.IF] = function(self, v)
    local t = self:compile_all(v[3])
    local start_jmp = is_jmp(t)
    local cond = self:compile_cond(v[2], false, start_jmp)
    local jmp = #t / 4 + 1

    if v[4] then
        local f = self:compile_all(v[4])
        t = t..o.JMP(self:jmp_reg(), #f / 4 + 1)..f
        jmp = jmp + 1
    end

    if not start_jmp then
        cond = cond..o.JMP(self:jmp_reg(), jmp)
    end
    return cond..t
end

compiler.comp[IR.CONDITIONAL] = function(self, v, r)
    local cond = self:compile_cond(v[2])
    r = r or self:reg()

    local t = self:compile(v[3], r)
    local f = self:compile(v[4], r)

    return cond
        .. o.JMP(self:jmp_reg(), #t / 4 + 2) .. t
        .. o.JMP(self:jmp_reg(), #f / 4 + 1) .. f, r
end

compiler.comp[IR.BREAK] = function(self)
    if not self.breaks then
        error("break not allowed outside loops") -- TODO: error
    end
    return "\xFF\xFF\xFF"..u8(self:jmp_reg())
end

local function loop_replace(code, len)
    if not len then
        len = #code / 4
    end

    return code:gsub("()(....)", function(pos, inst)
        local b = inst:byte(1)
        if b == 0xff then
            return o.JMP(inst:byte(4), len - (pos / 4))
        elseif b == opcodes.LOOP and inst:byte(3) == 0xff and inst:byte(4) == 0xff then
            return o.LOOP(inst:byte(2), len - (pos / 4))
        end

        return inst
    end)
end

compiler.comp[IR.LJ_LOOP] = function(self, _v)
    return o.LOOP(self:jmp_reg(), 0x8000)
end

compiler.comp[IR.LOOP] = function(self, v)
    local code = self:compile_all(v[2], true)
    local last = code:sub(-4, -1)

    if getop(last) == 50 then -- UCLO
        -- rewrite UCLO with jump target
        code = code:sub(1, -5)..o.UCLO(last:byte(2), -(#code / 4))
    else
        code = code..o.JMP(self:jmp_reg(), -(#code / 4))
    end

    return loop_replace(code)
end

compiler.comp[IR.NUMFOR] = function(self, v)
    local r0 = self:reg()
    local prep = self:compile(v[2], self:reg(r0, PUSH))
            .. self:compile(v[3], self:reg(r0+1, PUSH))
            .. self:compile(v[4], self:reg(r0+2, PUSH))

    self:reg(r0+3, PUSH)
    local off = self.local_offsets
    off[#off+1] = { r0, 3 }

    local code = self:compile_all(v[5], true)
    local len = #code / 4
    code = prep..o.FORI(r0, len + 2)..code..o.FORL(r0, -len)

    self:reg(r0, false)
    self:reg(r0+1, false)
    self:reg(r0+2, false)
    self:reg(r0+3, false)
    off[#off] = nil

    return loop_replace(code)
end

compiler.comp[IR.ITERFOR] = function(self, v)
    local r0 = self:reg()

    -- TODO: detect next/pairs and specialize to ISNEXT, ITERN
    local prep = self:compile(v[2], r0)

    for i=1,v[3] do
        self:reg(r0+2+i, PUSH)
    end

    local off = self.local_offsets
    off[#off+1] = { r0, 3 }

    local code = self:compile_all(v[4], true)
    local len = #code / 4
    code = prep
        .. o.JMP(self:jmp_reg(), len + 1)
        .. code
        .. o.ITERC(r0+3, v[3] + 1, 3)
        .. o.ITERL(r0+3, -(len + 1))

    for i=0,v[3]+2 do
        self:reg(r0+i, false)
    end
    off[#off] = nil
    return loop_replace(code)
end

local function binop(name)
    return function(self, v, r)
        local lhs, rhs = v[2], v[3]
        local ln = tconst(self, lhs, opcodes.KNUM)
        local rn = tconst(self, rhs, opcodes.KNUM)

        local a, b = "", ""
        local ra, rb = ln, rn

        if ln and not rn then
            ln = "N"
        else
            a, ra = self:compile(lhs)
            if shouldpop(self, ra) then
                r = r or ra
            end
            ln = "V"
        end

        if rn then
            rn = "N"
        else
            b, rb = self:compile(rhs)
            if shouldpop(self, rb) then
                r = r or rb
            end
            rn = "V"
        end
        r = r or self:reg()

        if ln == "N" and rn == "N" then
            error("oops?")
        end

        local s = a .. b .. (o[name .. ln .. rn](r, ra, rb))

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
        ra = ra or self:reg()
        local a, r = self:compile(v[2])
        local s = a..inst(ra, r)
        return s, ra
    end
end

compiler.comp[IR.ADD] = binop("ADD")
compiler.comp[IR.SUB] = binop("SUB")
compiler.comp[IR.MUL] = binop("MUL")
compiler.comp[IR.DIV] = binop("DIV")
compiler.comp[IR.MOD] = binop("MOD")

compiler.comp[IR.UNM] = unop(o.UNM)
compiler.comp[IR.LEN] = unop(o.LEN)
compiler.comp[IR.NOT] = unop(o.NOT)

compiler.cond[IR.NOT] = function(self, v, invert)
    return self:compile_cond(v[2], false, not invert)
end

compiler.comp[IR.CONCAT] = function(self, v, r)
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

    local s = table.concat(bc, "")..o.CAT(move or r, b, c)
    for i=move and b+1 or b, c do
        self.regs[i] = false
    end
    return s, r
end

local function bincmp(inst, inst_i, swap)
    return function(self, v, invert)
        local lhs, rhs = v[2], v[3]

        local a, ra = self:compile(lhs)
        local b, rb = self:compile(rhs)
        if swap then
            rb, ra = ra, rb
        end

        local s = a..b..(invert and inst_i or inst)(ra, rb)

        if shouldpop(self, rb) then
            self:reg(rb, false)
        end
        if shouldpop(self, ra) then
            self:reg(ra, false)
        end
        return s
    end
end

compiler.cond[IR.LT] = bincmp(o.ISGE, o.ISLT)
compiler.cond[IR.LTEQ] = bincmp(o.ISGT, o.ISLE)
compiler.cond[IR.GT] = bincmp(o.ISGE, o.ISLT, true)
compiler.cond[IR.GTEQ] = bincmp(o.ISGT, o.ISLE, true)

local function bineq(no)
    return function(self, v, invert)
        local lhs, rhs = v[2], v[3]

        -- (in)equality comparisons are swapped as
        -- needed to bring constants to the right
        if tpri(self, lhs) or tconst(self, lhs) then
            lhs, rhs = rhs, lhs
        end

        local ra
        lhs, ra = self:compile(lhs)

        local oper, rb
        if tpri(self, rhs) then
            rhs, rb = "", tpri(self, rhs)
            oper = "P"
        elseif tconst(self, rhs) then
            local n = tconst(self, rhs, opcodes.KNUM)
            if n then
                rhs, rb = "", n
                oper = "N"
            else
                n = tconst(self, rhs, opcodes.KSTR)
                if n then
                    rhs, rb = "", n
                    oper = "S"
                else
                    -- fallback to var
                    rhs, rb = self:compile(rhs)
                    oper = "V"
                end
            end
        else
            rhs, rb = self:compile(rhs)
            oper = "V"
        end

        if no then
            invert = not invert
        end
        local s = lhs..rhs..(o[(invert and "ISEQ" or "ISNE")..oper](ra, rb))

        if shouldpop(self, ra) then
            self:reg(ra, false)
        end
        if shouldpop(self, rb) then
            self:reg(rb, false)
        end
        return s
    end
end

compiler.cond[IR.EQ] = bineq(false)
compiler.cond[IR.NEQ] = bineq(true)

local function andor(c)
    return function(self, v, ra)
        ra = ra or self:reg()

        local lhs, rhs = v[2], v[3]

        -- optimize x = x or (y)
        if lhs[1] == IR.GETLOCAL and ra == self:localreg(lhs[2]) then
            -- ra = register of local
            local a = self:compile(rhs, ra)
            return (c and o.IST or o.ISF)(0, ra) -- check x
                .. o.JMP(self:jmp_reg(), #a / 4 + 1)
                .. a, ra -- set ra to rhs
        end

        local rb = self:reg()
        local b = self:compile(lhs, rb)
        self:reg(rb, false)
        local a = self:compile(rhs, ra)

        return b
            .. o.TESTSET(ra, rb, c)
            .. o.JMP(self:jmp_reg(), #a / 4 + 2)
            .. a, ra
    end
end

-- todo: optimize for truthness
local function andor_cond(c)
    return function(self, v, invert)        
        local lhs, rhs = v[2], v[3]
        lhs = self:compile_cond(lhs, false, c)
        rhs = self:compile_cond(rhs, false, invert)
        return lhs..o.JMP(0, #rhs / 4 + 2)..rhs
    end
end

compiler.comp[IR.AND] = andor(false)
compiler.comp[IR.OR] = andor(true)
compiler.cond[IR.AND] = andor_cond(false)
compiler.cond[IR.OR] = andor_cond(true)

function compiler:localreg(idx)
    for i=1,#self.local_offsets do
        local off = self.local_offsets[i]
        if idx >= off[1] then
            idx = idx + off[2]
        end
    end
    return idx
end

compiler.comp[IR.GETGLOBAL] = function(self, v, r)
    r = r or self:reg()
    return o.GGET(r, self.const_i[v[2]]), r
end

compiler.comp[IR.SETGLOBAL] = function(self, v, r)
    local code, a = self:compile(v[3], r)
    return code..o.GSET(a, self.const_i[v[2]]), a
end

compiler.comp[IR.GETLOCAL] = function(self, v, r)
    local l = self:localreg(v[2])
    if not r or r == l then
        print(v, l)
        return "", l
    end

    return o.MOV(r, l), r
end

compiler.comp[IR.SETLOCAL] = function(self, v, r)
    local n = self:localreg(v[2])
    if not r or r == n then
        return self:compile(v[3], n), n
    end

    local code = self:compile(v[3], r)
    return code..o.MOV(n, r), r
end

compiler.comp[IR.GETUPVAL] = function(self, v, r)
    r = r or self:reg()
    return o.UGET(r, v[2]), r
end

compiler.comp[IR.SETUPVAL] = function(self, v, r)
    if not r then
        local n = tconst(self, v[3], opcodes.KNUM)
        if n then
            return o.USETN(v[2], n)
        end

        n = tconst(self, v[3], opcodes.KSTR)
        if n then
            return o.USETS(v[2], n)
        end

        n = tpri(self, v[3])
        if n then
            return o.USETP(v[2], n)
        end
    end

    local b, rb = self:compile(v[3])
    return b..o.USETV(v[2], rb), rb
end

compiler.comp[IR.GETTABLE] = function(self, v, ra)
    local b, rb = self:compile(v[2], ra)
    ra = ra or self:reg()

    local n = tconst(self, v[3], opcodes.KNUM)
    if n and self.kshort[v[3][2]] >= 0 and self.kshort[v[3][2]] <= 255 then
        if shouldpop(self, rb) then
            self:reg(rb, false)
        end
        return b..o.TGETB(ra, rb, self.kshort[v[3][2]]), ra
    end

    n = tconst(self, v[3], opcodes.KSTR)
    if n then
        if shouldpop(self, rb) then
            self:reg(rb, false)
        end
        return b..o.TGETS(ra, rb, n), ra
    end

    local c, rc = self:compile(v[3])
    if shouldpop(self, rb) then
        self:reg(rb, false)
    end
    return b..c..o.TGETV(ra, rb, rc), ra
end

compiler.comp[IR.SETTABLE] = function(self, v, r)
    local b, rb = self:compile(v[2])
    local a, ra = self:compile(v[4], r)

    local n = tconst(self, v[3], opcodes.KNUM)
    if n and not r and self.kshort[v[3][2]] >= 0 and self.kshort[v[3][2]] <= 255 then
        if shouldpop(self, rb) then
            self:reg(rb, false)
        end
        return b..a..o.TSETB(ra, rb, self.kshort[v[3][2]]), ra
    end

    n = tconst(self, v[3], opcodes.KSTR)
    if n and not r then
        if shouldpop(self, rb) then
            self:reg(rb, false)
        end
        return b..a..o.TSETS(ra, rb, n), ra
    end

    local c, rc = self:compile(v[3])
    if shouldpop(self, rb) then
        self:reg(rb, false)
    end
    return b..c..a..o.TSETV(ra, rb, rc), ra
end

compiler.comp[IR.CALL] = function(self, v, a)
    local ra
    if a and v[3] == 1 and #v > 3 and a < self:jmp_reg()-1 then
        ra, a = a, self:reg()
    end

    a = a or self:reg()
    local target = self:compile(v[2], a)
    local c, nargs = 1, 0
    local bc = { target }
    if #v > 3 then
        for i=4,#v do
            local code = self:compile(v[i], self:reg(a + i - 3))
            bc[i - 2] = code
            c = i - 2
            nargs = nargs + 1
        end
    end

    bc[#bc+1] = o.CALL(a, v[3]+1, c)

    local nrets = v[3]
    local max = math.max(nargs+1, nrets)
    for i=0,max-1 do
        if shouldpop(self, a+i) then
            if DEBUG_REGS then
                print("call pop", a+i, i < nrets)
            end
            self.regs[a+i] = i < nrets
        end
    end

    if ra then
        bc[#bc+1] = o.MOV(ra, a)
        self:reg(a, false)
    end
    return table.concat(bc, ""), ra or a
end

compiler.comp[IR.NAMECALL] = function(self, v, a)
    local ra
    if a and v[3] == 1 and #v > 4 and a < self:jmp_reg()-1 then
        ra, a = a, self:reg()
    end

    a = a or self:reg()
    local from, target, b
    if v[2][1] == IR.GETLOCAL then
        from, b = "", self:localreg(v[2][2])
        target = o.MOV(a + 1, b)..o.TGETS(a, b, self.const_i[v[3]])
    else
        from, b = self:compile(v[2], a)
        target = o.MOV(a + 1, a)..o.TGETS(a, b, self.const_i[v[3]])
    end

    local c, nargs = 2, 1
    local bc = { from, target }
    if #v > 4 then
        for i=5,#v do
            local code = self:compile(v[i], self:reg(a + i - 3))
            bc[i - 2] = code
            c = i - 2
            nargs = nargs + 1
        end
    end

    bc[#bc+1] = o.CALL(a, v[4]+1, c)

    local nrets = v[4]
    local max = math.max(nargs+1, nrets)
    for i=0,max-1 do
        if DEBUG_REGS then
            print("call pop", a+i, i < nrets)
        end
        self.regs[a+i] = i < nrets
    end

    if ra then
        bc[#bc+1] = o.MOV(ra, a)
        self:reg(a, false)
    end
    return table.concat(bc, ""), a
end

function compiler:compile_chunk()
    if #self.irc.ir == 0 then
        self.code[1] = o.RET0(0, 1)
        return chunk(self)
    end

    for i,v in ipairs(self.irc.ir) do
        local code = self:compile(v)
        self.code[i] = code
        self.ninsts = self.ninsts + #code / 4
    end

    self.code[#self.code+1] = o.RET0(0, 1)
    self.ninsts = self.ninsts + 1

    if DEBUG_REGS then
        print("chunk end - first unused", self:jmp_reg())
    end

    return chunk(self)
end

-- selene: allow(unused_variable)
function compiler:compile_main(name)
    --self.name = name
    -- STRIP 0x02
    -- FR2   0x08
    local main = "\x1BLJ\x02" .. uleb128(2) .. self:compile_chunk() .. uleb128(0)

    if DEBUG_IR then
        print(self.irc)
    end

    return main
end

return { compiler=compiler, new_compiler=new_compiler }

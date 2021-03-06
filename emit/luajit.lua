-- spaghetti

local bit = require("bit")
local ffi = require("ffi")
local irc = require("emit.ir")
local base = require("emit.base")
local enum = require("common.utils").enum
local same = base.same
local IR = irc.IR

local bor, band, rshift, lshift = bit.bor, bit.band, bit.rshift, bit.lshift

local PUSH = base.PUSH

local i16_t = ffi.typeof('int16_t[1]')
local i32_t = ffi.typeof('int32_t[1]')
local double_t = ffi.typeof('double[1]')

local u8 = string.char
local function u16(num)
    return u8(band(num, 0xff))
        .. u8(band(rshift(num, 8), 0xff))
end
local function i32(num)
    return u8(band(num, 0xff))
        .. u8(band(rshift(num, 8), 0xff))
        .. u8(band(rshift(num, 16), 0xff))
        .. u8(band(rshift(num, 24), 0xff))
end
local function u64(num)
    return i32(num).."\0\0\0\0"
end

local function f64(num)
    return ffi.string(double_t({ num }), 8)
end
local function f64_b(num)
    local v = double_t({ num })
    local data = ffi.cast('uint8_t*', v)
    local lo, hi = i32_t(0), i32_t(0)
    ffi.copy(lo, data, 4)
    ffi.copy(hi, data + 4, 4)
    return lo[0], hi[0]
end

local p = p or function()end

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
    repeat
        local byte = band(num, 0x7f)
        num = rshift(num, 7)
        bytes[#bytes+1] = u8(num == 0 and byte or bor(byte, 0x80))
    until num <= 0
    return table.concat(bytes, "")
end

local function uleb128_33(num, b)
    local byte = u8(bor(lshift(band(num, 0x3f), 1), b)) -- ((num & 0x3f) << 1) | b
    local bytes = { byte }
    num = rshift(num, 6)
    while num > 0 do
        local byte = band(num, 0x7f)
        num = rshift(num, 7)
        bytes[#bytes+1] = u8(num == 0 and byte or bor(b, 0x80))
    end
    return table.concat(bytes, "")
end

local function is_int(num)
    return num == bor(0, num)
end

local function is_int16(num)
    return num == i16_t({ num })[0]
end

local size_ABC = 8
local size_D = 2 * size_ABC

local size_OP = 8
local mask_OP = lshift(1, size_OP)-1

local pos_OP = 0
local pos_A = pos_OP + size_OP
local pos_C = pos_A + size_ABC
local pos_B = pos_C + size_ABC
local pos_D = pos_C

local function iABC(o, a, b, c)
    return bor(lshift(o, pos_OP), lshift(a or 0, pos_A), lshift(b or 0, pos_B), lshift(c or 0, pos_C))
end

local function iAD(o, a, d)
    return bor(lshift(o, pos_OP), lshift(a or 0, pos_A), lshift(d or 0, pos_D))
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
    return s:gsub(".", function(c) return bit.tohex(string.byte(c)):sub(-2,-1) end)
end

local DEBUG_INSTS = true
local o = {}
for k,v in pairs(opcodes) do
    if type(k) == "number" then
        local mode = modes[k]
        local padded = v..(" "):rep(10-#v)
        local f = iAD
        if mode == BC_ABC then
            f = iABC
        elseif mode == BC_AJ then
            f = function(o, a, d)
                return iABx(o, a, d)
            end
        end

        if DEBUG_INSTS then
            o[v] = function(...)
                local s = i32(f(k, ...))
                print(padded, str_tohex(s), ...)
                return s
            end
        else
            o[v] = function(...)
                return i32(f(k, ...))
            end
        end
    end
end

local function getop(inst)
    return inst:byte(1)
end

local function is_jmp(inst)
    local o = getop(inst)
    return o == o.JMP or o == 255 -- 0xFF is break placeholder and replaced with JMP
end

local VKNIL, VKFALSE, VKTRUE = 0, 1, 2

local KOBJ = enum({
    [0] = "CHILD", "TAB", "I64", "U64", "COMPLEX", "STR",
}, false, true)

local KTAB = enum({
    [0] = "NIL", "FALSE", "TRUE", "INT", "NUM", "STR",
}, false, true)

-- see lj_bcwrite.c
local DEBUG_IPLINES = true
local function chunk(data, strip)
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
    local knum = reverse(data.knum)
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
local cond = {} -- conditions
local comp = setmetatable({}, { __index=base.base }) -- everything else
local compiler = { comp=comp, cond=cond }

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

    self.flags = 0x00
    for k,v in pairs(irc.protos) do
        self.flags = 0x01 -- PROTO_CHILD
        self.protos[k] = new_compiler(v):compile_chunk()
        self.kobj[#self.kobj+1] = uleb128(KOBJ.CHILD)
    end

    for k,v in pairs(irc.upvals) do
        local i = v.i
        if not v.upval then
            i = bor(i, v.mutable and 0x8000 or 0xc000)
        end

        print(i, type(i))
        self.upvals[k] = u16(i)
    end

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
            error()
        end
    end
    return self
end

local function shouldpop(self, r)
    return r < 255 and self.regs[r] ~= PUSH
end

local DEBUG_IR = false
function compiler:compile(ir, r, ...)
    if DEBUG_IR then print(ir) end
    local t = ir[1]
    local func = self.comp[t]
    if not func then
        if self.cond[t] then
            -- TODO
            return self:compile_cond(ir, r or true, false, ...)
        else
            error(("No luajit compile rule for %s"):format(ir[1])) -- TODO: error
        end
    end
    return func(self, ir, r, ...)
end

local DEBUG_REGS = false
function compiler:reg(r, v)
    if v == nil then v = true end
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

    for r=0,255 do
        if not self.regs[r] then
            if DEBUG_REGS then
                print("alloc", r, self.regs[r], "->", v)
            end
            self.regs[r] = v
            if self.maxstack <= r then
                self.maxstack = r+1
            end
            return r
        end
    end
    error("Could not allocate register")
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

compiler.comp[IR.CLOSURE] = function(self, v, r)
    r = r or self:reg()
    return o.FNEW(v[2]), r
end

-- check constant type
local function tconst(self, ir, t)
    if ir[1] ~= IR.CONST then return false end

    local ct = self.const_t[ir[2]]
    if ct == opcodes.KSHORT then ct = opcodes.KNUM end
    if ct == t then return self.const_i[ir[2]] end
    return false
end

compiler.comp[IR.RETURN] = function(self, v)
    local a, ra = self:compile(v[2])
    if shouldpop(self, ra) then
        self:reg(ra, false)
    end

    -- TODO: multiple returns
    return a..o.RET1(ra, 2)
end

-- TODO

local function binop(name)
    return function(self, v, r)
        local lhs, rhs = v[2], v[3]
        local ln = tconst(self, lhs, opcodes.KNUM)
        local rn = tconst(self, rhs, opcodes.KNUM)

        local a, b = "", ""
        local ra, rb = ln, rn
        if ln then
            ln = "N"
        else
            a, ra = self:compile(lhs, r)
            r = r or ra
            ln = "V"
        end
        if rn then
            rn = "N"
        else
            b, rb = self:compile(rhs)
            r = r or ra
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
    return function(self, v, r)
        local a, r = self:compile(v[2], r)
        local s = a..inst(r, r)
        return s, r
    end
end

compiler.comp[IR.ADD] = binop("ADD")
compiler.comp[IR.SUB] = binop("SUB")
compiler.comp[IR.MUL] = binop("MUL")
compiler.comp[IR.DIV] = binop("DIV")
compiler.comp[IR.MOD] = binop("MOD")

compiler.comp[IR.UNM] = unop("UNM")
compiler.comp[IR.LEN] = unop("LEN")
compiler.comp[IR.NOT] = unop("NOT") -- TODO: also add compiler.cond rule

function compiler:localreg(idx)
    for i=1,#self.local_offsets do
        local o = self.local_offsets[i]
        if idx >= o[1] then
            idx = idx + o[2]
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
    return o.GSET(a, self.const_i[v[2]]), a
end

compiler.comp[IR.GETLOCAL] = function(self, v, r)
    local l = self:localreg(v[2])
    if not r or r == l then
        return "", l
    else
        return o.MOV(r, l), r
    end
end

compiler.comp[IR.SETLOCAL] = function(self, v, r)
    local n = self:localreg(v[2])
    if not r or r == n then
        return self:compile(v[3], n)
    else
        local code, a = self:compile(v[3], r)
        return code..o.MOV(n, r), r
    end
end

compiler.comp[IR.GETTABLE] = function(self, v, ra)
    local b, rb = self:compile(v[2])
    ra = ra or rb
    local n = tconst(self, v[3], opcodes.KNUM)
    if n and self.kshort[v[3][2]] >= 0 and self.kshort[v[3][2]] <= 255 then
        return b..o.TGETB(ra, rb, self.kshort[v[3][2]]), ra
    end
    n = tconst(self, v[3], opcodes.KSTR)
    if n then
        return b..o.TGETS(ra, rb, n), ra
    end

    local c, rc = self:compile(v[3])
    return b..c..o.TGETV(ra, rb, rc), ra
end

compiler.comp[IR.SETTABLE] = function(self, v, ra)
    local a, ra = self:compile(v[4], ra)
    local b, rb = self:compile(v[2])
    local n = tconst(self, v[3], opcodes.KNUM)
    if n and self.kshort[v[3][2]] >= 0 and self.kshort[v[3][2]] <= 255 then
        return b..o.TGETB(ra, rb, self.kshort[v[3][2]]), ra
    end
    n = tconst(self, v[3], opcodes.KSTR)
    if n then
        return b..o.TGETS(ra, rb, n), ra
    end

    local c, rc = self:compile(v[3])
    return b..c..o.TGETV(ra, rb, rc), ra
end

compiler.comp[IR.CALL] = function(self, v, a)
    a = a or self:reg()
    local target, a = self:compile(v[2], a)
    local c, nargs = 1, 0
    local bc = { target }
    if #v > 3 then
        for i=4,#v do
            local code, r = self:compile(v[i], self:reg(a + i - 3))
            bc[i - 2] = code
            c = i - 2
            nargs = nargs + 1
        end
    end
    local nrets = v[3]
    local max = math.max(nargs+1, nrets)
    for i=0,max-1 do
        self.regs[a+i] = i < nrets
    end
    return table.concat(bc, "")..o.CALL(a, v[3]+1, c), a
end

compiler.comp[IR.NAMECALL] = function(self, v, a)
    local from, b = self:compile(v[2], self:reg(a + 1))
    local target = o.TGETS(a, b, self.const_i[v[3]])
    local c, nargs = 2, 1
    local bc = { from, target }
    if #v > 4 then
        for i=5,#v do
            local code, r = self:compile(v[i], self:reg(a + i - 3))
            bc[i - 2] = code
            c = i - 2
            nargs = nargs + 1
        end
    end
    local nrets = v[4]
    local max = math.max(nargs+1, nrets)
    for i=0,max-1 do
        self.regs[a+i] = i < nrets
    end
    return table.concat(bc, "")..o.CALL(a, v[4]+1, c), a
end

function compiler:compile_chunk()
    for i,v in ipairs(self.irc.ir) do
        local code, reg = self:compile(v)
        self.code[i] = code
        self.ninsts = self.ninsts + #code / 4
    end
    local last = self.code[#self.code]
    last = string.byte(last:sub(-4,-4))
    if last ~= opcodes.RET0 and last ~= opcodes.RET1 and last ~= opcodes.RET and last ~= opcodes.RETM then
        self.code[#self.code+1] = o.RET0(0, 1)
        self.ninsts = self.ninsts + 1
    end

    return chunk(self)
end

function compiler:compile_main(name)
    --self.name = name
    -- STRIP 0x02
    -- FR2   0x08
    local main = "\x1BLJ\x02" .. uleb128(2) .. self:compile_chunk() .. uleb128(0)
    p(main)
    return main
end

return { compiler=compiler, new_compiler=new_compiler }

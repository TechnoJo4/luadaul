-- Parser (expr/stmt) -> IR -> Bytecode Emitters
-- Resolution of locals/upvalues is done at this stage

local T = require("parse.token")
local ast = require("parse.ast")
local utils = require("parse.utils")

local ET = ast.expr_types
local enum = utils.enum

--[[
    Notes:
    (*) "POP" actually means "forget"/deallocate.
]]
local IR = enum({
    "NIL", "TRUE", "FALSE",
    "CONST",
    "GETTABLE", "SETTABLE",
    "GETLOCAL", "SETLOCAL",
    "GETUPVAL", "SETUPVAL",
    "GETGLOBAL", "SETGLOBAL",
    "ADD", "SUB",
    "MUL", "DIV", "MOD",
    "POW", "CONCAT",
    "UNM", "NOT", "LEN",
    "EQ", "NEQ",
    "LT", "LTEQ",
    "GT", "GTEQ",
    "CALL", "NAMECALL",
    "RETURN", "POP",
    "LOOP", "IF", "BREAK"
}, true)

local inst_meta = { __tostring=function(self)
    local t = {}
    for k,v in pairs(self) do
        t[k] = tostring(v)
    end
    return "("..table.concat(t, " ")..")"
end, __index={ __inst=true } }
local function inst(v)
    return setmetatable(v, inst_meta)
end

-- IR "compiler"
local irc = {}
local irc_meta = { __index=irc }
local function new_irc()
    local self = setmetatable({
        ir = {}, pos = 0,
        protos = {},
        depth = 0,
        constants = {}, nconstants = 0,
        locals = {}, nlocals = { [0]=0 }
    }, irc_meta)

    return self
end

function irc:start_scope()
    self.depth = self.depth + 1
    self.nlocals[self.depth] = 0
end

function irc:end_scope()
    local depth = self.depth
    local locals = self.locals
    if self.nlocals[depth] > 0 then
        local n = self.nlocals[depth]
        for i=1, n do
            self:emit({ IR.POP, n-i })
            locals[#locals] = nil
        end
    end
    self.nlocals[depth] = nil
    self.depth = depth - 1
end

function irc:resolve(name)
    local locals = self.locals
    for i=#locals,1,-1 do
        if locals[i] == name then
            return i-1
        end
    end
    return -1
end

function irc:constant(value, makeir)
    if self.constants[value] then
        return self.constants[value]
    end
    self.constants[value] = self.nconstants
    self.nconstants = self.nconstants + 1
    if makeir then
        return inst({ IR.CONST, self.nconstants-1 })
    end
    return self.nconstants-1
end

function irc:emit(ir)
    self.ir[#self.ir+1] = inst(ir)
end

function irc:expr(expr)
    local t = expr.type
    if not t then
        t = expr[1].type
        if t == T.Oper then
            t = expr[1].oper
        end
    end
    t = t or expr[1]

    local func = self[t]
    if not func then
        error(("No IR compile rule for %s"):format(t)) -- TODO: error
    end
    return inst(func(self, expr))
end

function irc:stmt(stmt)
    local func = self[stmt[1]]
    if not func then
        error(("No IR compile rule for %s"):format(stmt[1].type)) -- TODO: error
    end
    func(self, stmt)
end

function irc:declare(name)
    local n = 0
    for _,v in pairs(self.nlocals) do
        n = n + 1
    end
    if n >= 200 then
        error("Too many locals")
    end
    self.nlocals[self.depth] = self.nlocals[self.depth] + 1
    self.locals[#self.locals+1] = name
    return #self.locals-1
end

irc[ET.Expression] = function(self, stmt)
    local v = self:expr(stmt[2])
    if v[1] == IR.CALL then v[3] = 0 end
    if v[1] == IR.NAMECALL then v[4] = 0 end
    self:emit({ IR.POP, v })
end

irc[ET.Return] = function(self, stmt)
    self:emit({ IR.RETURN, self:expr(stmt[2]) })
end

irc[ET.Block] = function(self, stmt)
    self:start_scope()
    for _,v in ipairs(stmt.stmts) do
        self:stmt(v)
    end
    self:end_scope()
end

irc[ET.Declare] = function(self, stmt)
    self:emit({ IR.SETLOCAL, self:declare(stmt.name.name), self:expr(stmt.value) })
end

irc[T.Name] = function(self, tok)
    local r = self:resolve(tok.name)
    if r == -1 then
        return { IR.GETGLOBAL, self:constant(tok.name) }
    else
        return { IR.GETLOCAL, r }
    end
end

irc[T.Number] = function(self, v)
    return { IR.CONST, self:constant(v.num) }
end

irc[T.String] = function(self, v)
    return { IR.CONST, self:constant(v.data) }
end

irc[T.Nil] = function(self)
    return { IR.NIL }
end
irc[T.True] = function(self)
    return { IR.TRUE }
end
irc[T.False] = function(self)
    return { IR.FALSE }
end

local function unop(ir)
    return function(self, expr)
        return { ir, self:expr(expr[2]) }
    end
end
local function binop(ir)
    return function(self, expr)
        return { ir, self:expr(expr[2]), self:expr(expr[3]) }
    end
end
irc["+"] = binop(IR.ADD)
irc["*"] = binop(IR.MUL)
irc["/"] = binop(IR.DIV)
irc["%"] = binop(IR.MOD)
irc["^"] = binop(IR.POW)
irc["#"] = unop(IR.LEN)
irc["!"] = unop(IR.NOT)

irc["-"] = function(self, expr)
    if expr[3] then
        return { IR.SUB, self:expr(expr[2]), self:expr(expr[3]) }
    else
        if expr[2].type == T.Number then
            return { IR.CONST, self:constant(-expr[2].num) }
        end
        return { IR.UNM, self:expr(expr[2]) }
    end
end

irc[".."] = function(self, expr)
    local ir = { IR.CONCAT, self:expr(expr[2]) }
    local rhs = self:expr(expr[3])
    if rhs[1] == IR.CONCAT then
        for i=2,#rhs do
            ir[i+1] = rhs[i]
        end
    end
    return ir
end

irc[ET.Call] = function(self, expr)
    -- TODO: ir[3] should be nreturns
    local ir = { IR.CALL, self:expr(expr.target), 1 }
    for i,v in pairs(expr.args) do
        ir[i+3] = self:expr(v)
    end
    return ir
end

irc[ET.Namecall] = function(self, expr)
    -- TODO: ir[4] should be nreturns
    local ir = { IR.NAMECALL, self:expr(expr.from), self:constant(expr.target.name), 1 }
    for i,v in pairs(expr.args) do
        ir[i+4] = self:expr(v)
    end
    return ir
end

irc[ET.NameIndex] = function(self, expr)
    return { IR.GETTABLE, self:expr(expr.from), self:constant(expr.index.name, true) }
end

irc["="] = function(self, expr)
    local target = expr[2]
    if target.type == T.Name then
        local r = self:resolve(target.name)
        if r == -1 then
            return { IR.SETGLOBAL, self:constant(target.name), self:expr(expr[3]) }
        else
            return { IR.SETLOCAL, r, self:expr(expr[3]) }
        end
    else
        local ll = self:expr(target.from)
        local lr
        if target.type == ET.NameIndex then
            lr = self:constant(target.index.name, true)
        else -- ET.ExprIndex
            lr = self:expr(target.index)
        end
        return { IT.SETTABLE, ll, lr, self:expr(expr[3]) }
    end
end


return { IR=IR, irc=irc, new_irc=new_irc }

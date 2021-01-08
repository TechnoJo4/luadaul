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
        conversely, "PUSH"ing is actually allocating.
        The stack behaves like registers, but the
        allocation behaves like a stack anyways so this isn't actually crucial.
]]
local IR = enum({
    "NIL", "TRUE", "FALSE", "CONST",
    "GETTABLE", "SETTABLE",
    "GETLOCAL", "SETLOCAL",
    "GETUPVAL", "SETUPVAL",
    "GETGLOBAL", "SETGLOBAL",
    "ADD", "SUB",
    "MUL", "DIV", "MOD",
    "POW", "CONCAT",
    "UNM", "NOT", "LEN",
    "OR", "AND",
    "EQ", "NEQ",
    "LT", "LTEQ",
    "GT", "GTEQ",
    "CALL", "NAMECALL",
    "RETURN",
    "POP", "PUSH",
    "LOOP", "IF", "BREAK"
}, true)

local inst_meta = { __tostring=function(self)
    local t = {}
    for k,v in pairs(self) do
        if type(v) == "table" and (not getmetatable(v) or not getmetatable(v).__tostring) then
            local s = {}
            for i,v in pairs(v) do
                s[i] = tostring(v)
            end
            t[k] = "["..table.concat(s, ", ").."]"
        else
            t[k] = tostring(v)
        end
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

function irc:stmt(stmt, ret)
    local func = self[stmt[1]]
    if not func then
        error(("No IR compile rule for %s"):format(stmt[1])) -- TODO: error
    end
    if ret then
        local t = {}
        local old = self.emit
        self.emit = function(self, ir)
            t[#t+1] = inst(ir)
        end
        func(self, stmt)
        self.emit = old
        return t
    else
        func(self, stmt)
    end
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

irc[ET.Break] = function(self, stmt)
    self:emit({ IR.BREAK })
end

irc[ET.Block] = function(self, stmt)
    self:start_scope()
    for _,v in ipairs(stmt.stmts) do
        self:stmt(v)
    end
    self:end_scope()
end

irc[ET.Declare] = function(self, stmt)
    local r = self:declare(stmt.name.name)
    self:emit({ IR.PUSH, r })
    self:emit({ IR.SETLOCAL, r, self:expr(stmt.value) })
end

irc[ET.If] = function(self, stmt)
    self:emit({
        IR.IF,
        self:expr(stmt.cond),
        self:stmt(stmt.true_branch, true),
        stmt.false_branch and self:stmt(stmt.false_branch, true)
    })
end

irc[ET.Loop] = function(self, stmt)
    self:emit({ IR.LOOP, self:stmt(stmt[2], true) })
end

irc[ET.While] = function(self, stmt)
    local branch = self:stmt(stmt.branch, true)
    for i=#branch,1,-1 do
        branch[i+1] = branch[i]
    end
    branch[1] = inst({ IR.IF, inst({ IR.NOT, self:expr(stmt.cond) }), { inst({ IR.BREAK }) } })
    self:emit({ IR.LOOP, branch })
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

irc["||"] = binop(IR.OR)
irc["&&"] = binop(IR.AND)
irc["=="] = binop(IR.EQ)
irc["!="] = binop(IR.NEQ)
irc["<"]  = binop(IR.LT)
irc["<="] = binop(IR.LTEQ)
irc[">"]  = binop(IR.GT)
irc[">="] = binop(IR.GTEQ)

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
        return { IR.SETTABLE, ll, lr, self:expr(expr[3]) }
    end
end


return { IR=IR, irc=irc, new_irc=new_irc }

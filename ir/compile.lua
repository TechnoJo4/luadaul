-- Parser -> IR (you are here) -> Bytecode Emitters

local T = require("parse/token")
local ast = require("parse/ast")
local opts = require("ir/optimize")
local IR = require("ir/insts")

local ET = ast.expr_types

local inst_meta = { __tostring=function(self)
    local t = {}
    for k,v in pairs(self) do
        if type(v) == "table" and (not getmetatable(v) or not getmetatable(v).__tostring) then
            local s = {}
            for i,v2 in pairs(v) do
                s[i] = tostring(v2)
            end
            t[k] = "["..table.concat(s, ", ").."]"
        else
            t[k] = tostring(v)
        end
    end
    return "("..table.concat(t, " ")..")"
end, __index={ __inst=true } }

local function irc_tostr(self, lvl) -- for debug logging
    lvl = lvl or 0

    local protos = {}
    for k,v in pairs(self.protos) do
        protos[k] = irc_tostr(v, lvl+1)
    end
    local constants = {}
    for k,v in pairs(self.constvals) do
        constants[v+1] = tostring(k)
    end
    local upvals = {}
    for k,v in pairs(self.upvals) do
        upvals[k] = "(name=" .. v.name
            .. ", i=" .. tostring(v.i)
            .. ", upval=" .. tostring(v.upval) .. ")"
    end
    local ir = {}
    for k,v in pairs(self.ir) do
        ir[k] = "    "..tostring(v)
    end

    if #protos > 0 then
        protos[#protos] = protos[#protos].."\n"
    end
    local indent = "\n"..("    "):rep(lvl)
    local _s = "Prototypes: ["..table.concat(protos, ",")
            .. "],\nConstants: ["..table.concat(constants, ", ")
            .. "],\nUpvalues: ["..table.concat(upvals, ", ")
            .. "],\nCode: [\n"..table.concat(ir, "\n") .. "\n]"
    return indent.._s:gsub("\n", indent)
end

-- IR "compiler"
local irc = {}
local irc_meta = { __index=irc, __tostring=irc_tostr }
local function new_irc(parent, args)
    local nparams = args and #args or 0
    local self = setmetatable({
        ir = {},
        protos = {}, parent=parent,
        constants = {}, constvals = {}, nconstants = 0,
        upvals = {}, nupvals = 0,
        locals = {}, nlocals = { [0] = nparams }, depth = 0,
        nparams=nparams
    }, irc_meta)

    if args and #args > 0 then
        for _, v in ipairs(args) do
            self.ir[#self.ir+1] = self:inst({ IR.PUSH, self:declare(v.name) })
        end
    end

    self:start_scope()
    return self
end

function irc:inst(v)
    return setmetatable(opts.inst(v, self), inst_meta)
end

function irc:finalize()
    local code = { self:end_scope() }
    for i,v in ipairs(code) do
        code[i] = self:inst(v)
    end
    self:emit(code)

    opts.final(self)
end

function irc:start_scope()
    self.depth = self.depth + 1
    self.nlocals[self.depth] = 0
end

function irc:end_scope()
    local depth = self.depth
    local locals = self.locals
    local code = {}
    if self.nlocals[depth] > 0 then
        local close = { IR.CLOSE }
        local pop = { IR.POP }

        for _=1,self.nlocals[depth] do
            local l = locals[#locals]
            local a = l.close and close or pop
            a[#a+1] = l.i
            locals[#locals] = nil
        end

        if #pop > 1 then
            code[#code+1] = pop
        end
        if #close > 1 then
            code[#code+1] = close
        end
    end
    self.nlocals[depth] = nil
    self.depth = depth - 1
    return unpack(code)
end

function irc:declare(name)
    local n = 0
    for _,v in pairs(self.nlocals) do
        n = n + v
    end
    if n >= 200 then
        error("Too many locals")
    end
    self.nlocals[self.depth] = self.nlocals[self.depth] + 1
    self.locals[#self.locals+1] = { name=name, i=#self.locals, close=false }
    return #self.locals-1
end

function irc:resolve(name, set, new_uv)
    for i=#self.locals,1,-1 do
        if self.locals[i].name == name then
            if new_uv then
                self.locals[i].close = true
            end
            if set then
                self.locals[i].mutable = true
            end

            return set and IR.SETLOCAL or IR.GETLOCAL, i-1
        end
    end

    for i=#self.upvals,1,-1 do
        local u = self.upvals[i]
        if u.name == name then
            if set then
                u.l.mutable = true
                return IR.SETUPVAL, i-1
            end
            return IR.GETUPVAL, i-1
        end
    end

    if self.parent then
        local ir, i = self.parent:resolve(name, set, true)
        if i then
            local ir2 = set and IR.SETUPVAL or IR.GETUPVAL
            if ir == ir2 then
                self.upvals[#self.upvals+1] = { name=name, i=i, upval=true, l=self.parent.upvals[i+1].l }
            else
                self.upvals[#self.upvals+1] = { name=name, i=i, upval=false, l=self.parent.locals[i+1] }
            end
            self.nupvals = self.nupvals + 1
            return ir2, self.nupvals-1
        end
    end

    return set and IR.SETGLOBAL or IR.GETGLOBAL
end

function irc:constant(value, makeir)
    if self.constvals[value] then
        local n = self.constvals[value]
        if makeir then
            return self:inst({ IR.CONST, n })
        end
        return n
    end
    local n = self.nconstants
    self.nconstants = self.nconstants + 1
    self.constvals[value] = n
    self.constants[n] = value
    if makeir then
        return self:inst({ IR.CONST, n })
    end
    return n
end

function irc:expr(expr)
    local t = expr.type
    if not t then
        t = type(expr[1]) == "table" and expr[1].type
        if t == T.Oper then
            t = expr[1].oper
        end
    end
    t = t or expr[1]

    local func = self[t]
    if not func then
        error(("No IR compile rule for %s"):format(t)) -- TODO: error
    end
    return self:inst(func(self, expr))
end

function irc:emit(code)
    local i = #self.ir
    for j,v in ipairs(code) do
        self.ir[i+j] = v
    end
end

function irc:stmt(stmt, ret)
    local func = self[stmt[1]]
    if not func then
        error(("No IR compile rule for %s"):format(stmt[1])) -- TODO: error
    end

    local code = { func(self, stmt) }
    for i,v in pairs(code) do
        code[i] = self:inst(v)
    end
    if ret then
        return unpack(code)
    else
        self:emit(code)
    end
end

function irc:closure(args)
    local n = 0
    for _,_ in pairs(self.protos) do
        n = n + 1
    end
    if n >= 255 then
        error("Too many prototypes")
    end
    local new = new_irc(self, args)
    self.protos[#self.protos+1] = new
    return new, #self.protos-1
end

irc[ET.Expression] = function(self, stmt)
    local v = self:expr(stmt[2])
    if v[1] == IR.CALL then
        v[3] = 0
    end
    if v[1] == IR.NAMECALL then
        v[4] = 0
    end
    return { IR.POP, v }
end

irc[ET.Return] = function(self, stmt)
    local min
    for i=#self.locals,1,-1 do
        if self.locals[i].close then
            min = i - 1
        end
    end
    return { IR.RETURN, self:expr(stmt[2]), min }
end

irc[ET.Break] = function(self, _stmt)
    return { IR.BREAK }
end

irc[ET.Block] = function(self, block)
    local c = {}
    self:start_scope()
    local i = 1
    for _,stmt in ipairs(block.stmts) do
        local ir = { self:stmt(stmt, true) }
        for _,v in ipairs(ir) do
            c[i] = v
            i = i + 1
        end
    end
    c[i] = self:end_scope()
    return unpack(c)
end

irc[ET.Declare] = function(self, stmt)
    local r = self:declare(stmt.name.name)
    return { IR.PUSH, r }, { IR.SETLOCAL, r, self:expr(stmt.value) }
end

irc[ET.If] = function(self, stmt)
    local t = { self:stmt(stmt.true_branch, true) }
    local f = stmt.false_branch and { self:stmt(stmt.false_branch, true) }
    return { IR.IF, self:expr(stmt.cond), t, f }
end

irc[ET.ForNum] = function(self, stmt)
    local start = self:expr(stmt.start)
    local stop = self:expr(stmt.stop)
    local step = stmt.step and self:expr(stmt.step) or self:constant(1, true)

    self:declare(stmt.name.name)
    local stmts = { self:stmt(stmt.body, true) }
    self.locals[#self.locals] = nil
    self.nlocals[self.depth] = self.nlocals[self.depth] - 1

    return { IR.NUMFOR, start, stop, step, stmts }
end

irc[ET.ForIter] = function(self, stmt)
    local iter = self:expr(stmt.iter)
    if iter[1] == IR.CALL then
        iter[3] = 3
    elseif iter[1] == IR.NAMECALL then
        iter[4] = 3
    end

    for _,name in pairs(stmt.names) do
        self:declare(name.name)
    end

    local stmts = { self:stmt(stmt.body, true) }

    for _,_ in pairs(stmt.names) do
        self.locals[#self.locals] = nil
        self.nlocals[self.depth] = self.nlocals[self.depth] - 1
    end

    return { IR.ITERFOR, iter, #stmt.names, stmts }
end

irc[ET.Loop] = function(self, stmt)
    return { IR.LOOP, {
        self:inst({ IR.LJ_LOOP }),
        self:stmt(stmt[2], true)
    } }
end

irc[ET.While] = function(self, stmt)
    return { IR.LOOP, {
        self:inst({
            IR.IF,
            self:inst({ IR.NOT, self:expr(stmt.cond) }), {
                self:inst({ IR.BREAK }),
                self:inst({ IR.LJ_LOOP }) -- is compiled away on lua51
            }
        }),
        self:stmt(stmt.body, true)
    } }
end

irc[ET.Lambda] = function(self, stmt)
    local closure_irc, n = self:closure(stmt.args)
    for _,v in ipairs(stmt.stmts) do
        closure_irc:stmt(v)
    end
    local code = { IR.CLOSURE, n }
    for i,v in ipairs(closure_irc.upvals) do
        code[i+2] = self:inst({ v.upval and IR.GETUPVAL or IR.GETLOCAL, v.i })
    end
    return code
end

irc[T.Name] = function(self, tok)
    local ir, r = self:resolve(tok.name)
    if not r then
        r = self:constant(tok.name)
    end
    return { ir, r }
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

irc[ET.TableKey] = function(self, expr)
    return { IR.CONST, self:constant(expr[2]) }
end

irc[ET.Table] = function(self, expr)
    local ir = { IR.NEWTABLE }

    -- get fields with number keys
    local t = {}
    for _,v in ipairs(expr.fields) do
        local key = v[1]
        if key[1] == ET.TableKey and type(key[2]) == "number" then
            t[key[2]] = { key, self:constant(key[2]), self:compile(v[2]) }
        end
    end

    local inarray = {}
    local array, hash = 0, 0

    -- ipairs to only iterate through contiguous keys starting from 1
    for _,v in ipairs(t) do
        inarray[v[1]] = true
        array = array + 1
    end

    -- get fields not in array
    for _,v in pairs(expr.fields) do
        if not inarray[v[1]] then
            hash = hash + 1
        end
    end

    ir[2] = array
    ir[3] = hash
    ir[4] = expr.fields
    return ir
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
    else
        ir[3] = rhs
    end
    return ir
end

irc[ET.Call] = function(self, expr)
    -- ir[3] is number of expected returns, modified by other parts of the IR compiler
    local ir = { IR.CALL, self:expr(expr.target), 1 }
    for i,v in pairs(expr.args) do
        ir[i+3] = self:expr(v)
    end
    return ir
end

irc[ET.Namecall] = function(self, expr)
    -- ir[4] is number of expected returns, modified by other parts of the IR compiler
    local ir = { IR.NAMECALL, self:expr(expr.from), self:constant(expr.target.name), 1 }
    for i,v in pairs(expr.args) do
        ir[i+4] = self:expr(v)
    end
    return ir
end

irc[ET.ExprIndex] = function(self, expr)
    return { IR.GETTABLE, self:expr(expr.from), self:expr(expr.index) }
end

irc[ET.NameIndex] = function(self, expr)
    return { IR.GETTABLE, self:expr(expr.from), self:constant(expr.index.name, true) }
end

irc["="] = function(self, expr)
    local target = expr[2]
    if target.type == T.Name then
        local ir, r = self:resolve(target.name, true)
        if not r then
            r = self:constant(target.name)
        end
        return { ir, r, self:expr(expr[3]) }
    else
        local ll = self:expr(target.from)
        local lr
        if target[1] == ET.NameIndex then
            lr = self:constant(target.index.name, true)
        else -- ET.ExprIndex
            lr = self:expr(target.index)
        end
        return { IR.SETTABLE, ll, lr, self:expr(expr[3]) }
    end
end

irc[ET.Conditional] = function(self, expr)
    return {
        IR.CONDITIONAL,
        self:expr(expr.cond),
        self:expr(expr.true_branch),
        self:expr(expr.false_branch)
    }
end

return { IR=IR, irc=irc, new_irc=new_irc }

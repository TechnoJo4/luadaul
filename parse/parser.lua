local ast = require("parse.ast")
local utils = require("parse.utils")
local lexer = require("parse.lexer")
local stream = require("parse.stream")

local enum = utils.enum
local T = lexer.token_types
local ET = ast.expr_types
local expr = ast.expr

local r_opers_arr = { "^", "#", "*", "%", "/", "+", "-", "<", "<=", ">", ">=", "|>", "->", ".", "," }
local r_opers = enum(r_opers_arr)

-- Precedence enum
-- TODO: make indicies sparse so user-defined operators' precedences
-- can be between builtin operators' precedences
local P = enum({
    "Assignment",
    "Pipeline",
    "Or",
    "And",
    "Equality",
    "Comparison",
    "Elvis",
    "Concat",
    "Addition",
    "Multiplication",
    "Unary",
    "Power",
    "Primary"
})

local function identity(parser, token)
    return token
end

local function prefix(parser, token)
    return expr({ token, parser:expr(P.Unary) })
end

local function left(parser, token, left, prec)
    return expr({ token, left, parser:expr(prec) })
end

local function right(parser, token, left, prec)
    return expr({ token, left, parser:expr(prec - 1) })
end

local function consume(lexer, ttype)
    local token = lexer.adv()
    if token.type ~= ttype then
        print(token)
        error("error (consume)") -- TODO: error
    end
    return token
end

local function match(lexer, ttype)
    local token = lexer.get(1)
    if token.type == ttype then
        return lexer.adv()
    end
    return false
end

local function consume_oper(lexer, oper)
    local token = lexer.adv()
    if token.type ~= T.Oper or token.oper ~= oper then
        print(token)
        error("error (consume_oper)") -- TODO: error
    end
    return token
end

local function typename(lexer)
    local token = lexer.adv()
    if token.type ~= T.Name or not token.typevalid then
        print(token)
        error("error (typename)") -- TODO: error
    end
    return token
end

local parser = {}
local parser_meta = { __index=parser }
function new_parser(source)
    local self = setmetatable({
        lexer = stream(lexer.get_next, source),
        stmts = {}, pre = {}, post = {}
    }, parser_meta)

    self:def_pre(T.Nil, identity)
    self:def_pre(T.True, identity)
    self:def_pre(T.False, identity)
    self:def_pre(T.Number, identity)
    self:def_pre(T.Name, identity)
    -- TODO: ...

    self:def_pre("-", prefix)
    self:def_pre("#", prefix)
    self:def_pre("!", prefix)

    self:def_post("||", P.Or, left)
    self:def_post("&&", P.And, left)
    self:def_post("==", P.Equality, left)
    self:def_post("!=", P.Equality, left)
    self:def_post("<", P.Comparison, left)
    self:def_post(">", P.Comparison, left)
    self:def_post("<=", P.Comparison, left)
    self:def_post(">=", P.Comparison, left)
    self:def_post("?:", P.Elvis, left)
    self:def_post("..", P.Concat, right)
    self:def_post("+", P.Addition, left)
    self:def_post("-", P.Addition, left)
    self:def_post("/", P.Multiplication, left)
    self:def_post("*", P.Multiplication, left)
    self:def_post("%", P.Multiplication, left)
    self:def_post("^", P.Power, right)

    local _assignables = enum({ ET.ExprIndex, ET.NameIndex, T.Name })
    self:def_post("=", P.Assignment, function(parser, token, left, prec)
        local t = left.type or left[1].type
        if not _assignables[t == T.Oper and left[1].oper or t] then
            -- TODO: error
            error("Invalid assignment target")
        end

        return expr({ token, left, parser:expr(prec-1) })
    end)

    local function call(parser)
        local i = 1
        local args = {}
        if not match(parser.lexer, T.RPar) then
            repeat
                args[i] = parser:expr()
                i = i + 1
            until not match(parser.lexer, T.Comma)
            consume(parser.lexer, T.RPar)
        end
        -- TODO: Trailing lambda call

        return args
    end

    self:def_post(".", P.Primary, function(parser, token, left)
        return expr({
            ET.NameIndex,
            token,
            from=left,
            index=consume(parser.lexer, T.Name)
        })
    end)
    self:def_post(":", P.Primary, function(parser, token, left)
        local name = consume(parser.lexer, T.Name)
        consume(parser.lexer, T.LPar)
        return expr({
            ET.Namecall,
            from=left,
            target=name,
            args=call(parser),
            token
        })
    end)
    self:def_post(T.LPar, P.Primary, function(parser, token, left)
        return expr({
            ET.Call,
            target=left,
            args=call(parser),
            token
        })
    end)
    self:def_post("|>", P.Pipeline, function(parser, token, left)
        local e = parser:expr(P.Primary - 1)
        local t = e[1]
        if t ~= ET.Call and t ~= ET.Namecall then
            error("Invalid pipeline call target.")
        end
        local args = e.args
        for i=#args,1,-1 do
            args[i+1] = args[i]
        end
        args[1] = left
        return e
    end)

    self:def_post(T.LSQB, P.Primary, function(parser, token, left)
        return expr({
            ET.ExprIndex,
            token,
            from=left,
            index=parser:expr(),
            consume(parser.lexer, T.RSQB)
        })
    end)

    self:def_pre(T.LPar, function(parser, token)
        local e = parser:expr()
        consume(parser.lexer, T.RPar)
        return e
    end)

    self:def_stmt(T.Let, function(parser, token)
        -- TODO: function declaration
        local name = consume(parser.lexer, T.Name)
        consume_oper(parser.lexer, "=")
        local value = parser:expr()
        consume(parser.lexer, T.Semi)
        return expr({ ET.Declare, name=name, value=value })
    end)

    self:def_stmt(T.LBrace, function(parser, token)
        local stmts = {}
        local i = 1
        while not match(parser.lexer, T.RBrace) do
            stmts[i] = parser:stmt()
            i = i + 1
        end
        return expr({ ET.Block, stmts=stmts })
    end)

    return self
end

function parser:stmt()
    local token = self.lexer.get(1)
    local func = self.stmts[token.type]
    local stmt
    if func then
        self.lexer.adv()
        stmt = func(self, token)
    else
        stmt = expr({ ET.Expression, self:expr() })
        consume(self.lexer, T.Semi)
    end
    return stmt
end

function parser:expr(prec)
    if not prec then prec = 0 end

    local token = self.lexer.adv()
    local idx = token.type
    if token.type == T.Oper then
        idx = token.oper
    end
    local func = self.pre[idx]
    if not func then
        error(("No parse rule for %s"):format(idx)) -- TODO: error
    end

    local left = func(self, token, prec)
    repeat
        token = self.lexer.get(1)
        if token.type == T.EOF then
            break
        end
        idx = token.type
        if token.type == T.Oper then
            idx = token.oper
        elseif token.type == T.Name and not token.typevalid then
            idx = token.name
        end

        local rule = self.post[idx]
        if not rule or rule.prec <= prec then break end

        self.lexer.adv()
        left = rule.func(self, token, left, rule.prec)
    until false

    return left
end

function parser:def_stmt(tok, func)
    self.stmts[tok] = func
end

function parser:def_pre(tok, func)
    self.pre[tok] = func
end

function parser:def_post(tok, prec, func)
    self.post[tok] = { prec=prec, func=func }
end

return { new_parser=new_parser, parser=parser }

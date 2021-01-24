local ast = require("parse.ast")
local lexer = require("parse.lexer")
local stream = require("parse.stream")
local utils = require("common.utils")

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
    "Coalescing",
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

local function match(lexer, ttype)
    local i = 1
    local token = lexer.get(i)
    if ttype ~= T.Newline then
        while token.type == T.Newline do
            i = i + 1
            token = lexer.get(i)
        end
    end
    if token.type == ttype then
        return lexer.adv(i)
    end
    return false
end

local function consume(lexer, ttype)
    local token = lexer.adv()
    if ttype ~= T.Newline then
        while token.type == T.Newline do
            token = lexer.adv()
        end
    end
    if token.type ~= ttype then
        print("prev", lexer.get(-1), "cur", token, "expected", ttype)
        error("error (consume)") -- TODO: error
    end
    return token
end

local function consume_end(lexer)
    local token = lexer.adv()
    if token.type ~= T.Newline and token.type ~= T.Semi then
        print("prev", lexer.get(-1), "cur", token)
        error("error (consume_end)") -- TODO: error
    end
    return token
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
    self:def_pre(T.String, identity)

    self:def_pre("-", prefix)
    self:def_pre("#", prefix)
    self:def_pre("!", prefix)

    self:def_post("||", P.Or, left, true)
    self:def_post("&&", P.And, left, true)
    self:def_post("==", P.Equality, left)
    self:def_post("!=", P.Equality, left)
    self:def_post("<", P.Comparison, left)
    self:def_post(">", P.Comparison, left)
    self:def_post("<=", P.Comparison, left)
    self:def_post(">=", P.Comparison, left)
    self:def_post("??", P.Coalescing, left, true)
    self:def_post("..", P.Concat, right, true)
    self:def_post("+", P.Addition, left, true)
    self:def_post("-", P.Addition, left, true)
    self:def_post("/", P.Multiplication, left, true)
    self:def_post("*", P.Multiplication, left, true)
    self:def_post("%", P.Multiplication, left, true)
    self:def_post("^", P.Power, right, true)

    self:def_pre(T.If, function(parser, token)
        local s = expr({ ET.Conditional })
        consume(parser.lexer, T.LPar)
        s.cond = parser:expr()
        consume(parser.lexer, T.RPar)
        s.true_branch = parser:expr()
        consume(parser.lexer, T.Else)
        s.false_branch = parser:expr()
        return s
    end)

    local _assignables = enum({ ET.ExprIndex, ET.NameIndex, T.Name })
    self:def_post("=", P.Assignment, function(parser, token, left, prec)
        local t = left.type or left[1].type or left[1]
        if not _assignables[t == T.Oper and left[1].oper or t] then
            -- TODO: error
            print(left, t)
            error("Invalid assignment target")
        end

        return expr({ token, left, parser:expr(prec-1) })
    end)

    local function block_lambda(parser)
        local i = 0
        local _match = match
        local function matchr(...)
            local m = _match(parser.lexer, ...)
            if m then
                i = i + 1
            end
            return m
        end

        -- parse args
        local args = {}
        while true do
            local name = matchr(T.Name)
            if not name then
                args = nil
                parser.lexer.recede(i)
                break
            end
            args[#args+1] = name

            local m = matchr(T.Oper)
            if m and m.oper == "->" then
                break
            end
            if m or not matchr(T.Comma) then
                args = nil
                parser.lexer.recede(i)
                break
            end
        end

        -- parse body
        matchr, i = nil, 1
        local stmts = {}
        if not match(parser.lexer, T.RBrace) then
            while true do
                stmts[i] = parser:stmt(false, true)
                i = i + 1
                if match(parser.lexer, T.RBrace) then
                    break
                else
                    consume_end(parser.lexer)
                end
            end
        end

        if #stmts >= 1 then
            local s = stmts[#stmts]
            if s[1] == ET.Expression then
                s[1] = ET.Return
            end
        end
        return { ET.Lambda, args=args, stmts=stmts }
    end
    self:def_pre(T.LBrace, block_lambda)

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
        if match(parser.lexer, T.LBrace) then
            args[i] = block_lambda(parser)
        end

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
        local args
        if match(parser.lexer, T.LBrace) then
            args = { block_lambda(parser) }
        else
            consume(parser.lexer, T.LPar)
            args = call(parser)
        end
        return expr({
            ET.Namecall,
            from=left,
            target=name,
            args=args,
            token
        })
    end)
    self:def_post(T.LPar, P.Primary, function(parser, token, left)
        return expr({
            ET.Call,
            target=left,
            args=call(parser)
        })
    end)
    self:def_post(T.LBrace, P.Primary, function(parser, token, left)
        return expr({
            ET.Call,
            target=left,
            args={ block_lambda(parser) }
        })
    end, false, true)
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
    -- TODO: pipeline assignment

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

    self:def_stmt(T.Let, function(parser, token, no_end)
        -- TODO: function declaration
        local name = consume(parser.lexer, T.Name)
        consume_oper(parser.lexer, "=")
        local value = parser:expr()
        if not no_end then
            consume_end(parser.lexer)
        end
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

    self:def_stmt(T.Return, function(parser, token, no_end)
        local e = parser:expr()
        if not no_end then
            consume_end(parser.lexer)
        end
        return expr({ ET.Return, e })
    end)

    self:def_stmt(T.Do, function(parser, token)
        return parser:stmt(true)
    end)

    self:def_stmt(T.If, function(parser, token, no_end)
        local s = expr({ ET.If })
        consume(parser.lexer, T.LPar)
        s.cond = parser:expr()
        consume(parser.lexer, T.RPar)
        s.true_branch = parser:stmt(true, true)
        if match(parser.lexer, T.Else) then
            s.false_branch = parser:stmt(true, true)
        end
        if not no_end then
            consume_end(parser.lexer)
        end
        return s
    end)

    self:def_stmt(T.While, function(parser, token, no_end)
        local s = expr({ ET.While })
        consume(parser.lexer, T.LPar)
        s.cond = parser:expr()
        consume(parser.lexer, T.RPar)
        s.body = parser:stmt(true)
        return s
    end)

    self:def_stmt(T.For, function(parser, token)
        consume(parser.lexer, T.LPar)
        local name = consume(parser.lexer, T.Name)
        local comma = match(parser.lexer, T.Comma)

        if comma then
            name = { name }
            repeat
                name[#name+1] = consume(parser.lexer, T.Name)
            until not match(parser.lexer, T.Comma)
            consume(parser.lexer, T.In)
        end

        if comma or match(parser.lexer, T.In) then
            local s = expr({ ET.ForIter })
            if not comma then name = { name } end
            s.names = name
            s.iter = parser:expr() -- TODO: that thing
            consume(parser.lexer, T.RPar)

            s.body = parser:stmt(true)
            return s
        else
            local s = expr({ ET.ForNum })
            s.start = parser:expr()
            consume(parser.lexer, T.Comma)
            s.stop = parser:expr()
            if match(parser.lexer, T.Comma) then
                s.step = parser:expr()
            end
            consume(parser.lexer, T.RPar)

            s.body = parser:stmt(true)
            return s
        end
    end)

    self:def_stmt(T.Loop, function(parser, token, no_end)
        return expr({ ET.Loop, parser:stmt(true) })
    end)

    self:def_stmt(T.Break, function(parser, token)
        return expr({ ET.Break })
    end)

    return self
end

function parser:stmt(allow_block, no_end)
    local token = self.lexer.adv()
    while token.type == T.Newline do
        token = self.lexer.adv()
    end
    local func = self.stmts[token.type]
    local stmt
    if func and (allow_block or token.type ~= T.LBrace) then
        stmt = func(self, token, no_end)
    else
        self.lexer.recede(1)
        stmt = expr({ ET.Expression, self:expr() })
        if not no_end then
            consume_end(self.lexer)
        end
    end
    return stmt
end

function parser:expr(prec)
    if not prec then prec = P.Assignment-1 end

    local token = self.lexer.adv()
    while token.type == T.Newline do
        token = self.lexer.adv()
    end
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
        local i = 1
        token = self.lexer.get(i)
        while token.type == T.Newline do
            i = i + 1
            token = self.lexer.get(i)
        end
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
        if not rule or rule.prec+0 <= prec+0 or (i > 1 and not rule.skipnl) then break end

        self.lexer.adv(i)
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

function parser:def_post(tok, prec, func, create_assign_oper, no_nl)
    self.post[tok] = { prec=prec, func=func, skipnl=(not no_nl) }
    if create_assign_oper then
        if type(tok) ~= "string" then
            error("Attempt to define a post rule with create_assign_oper for a non-oper token type "..tostring(tok))
        end
        tok = tok .. "="
        self.post[tok] = { prec=P.Assignment, func=function(parser, token, left, prec)
            return expr({ ET.OperAssign, token, left, parser:expr(prec - 1) })
        end }
    end
end

return { new_parser=new_parser, parser=parser }

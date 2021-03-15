local utils = require("common.utils")
local ast = require("parse.ast")

local P = require("parse.precedence")
local T = require("parse.token")
local ET = ast.expr_types

local enum = utils.enum
local expr = ast.expr

local DEBUG_ERRORS = true

local errors = require("common.errors")
local error_t = errors.types.syntax_error
local function err(o, ...)
    errors.at_token(o.source, ...)
    if DEBUG_ERRORS then
        print(debug.traceback("", 2))
    end
    os.exit(1)
end

local function identity(_parser, token)
    return token
end

local function prefix(parser, token)
    return expr({ token, parser:expr(P.Unary) })
end

local function binop_left(parser, token, left, prec)
    return expr({ token, left, parser:expr(prec) })
end

local function binop_right(parser, token, left, prec)
    return expr({ token, left, parser:expr(prec - 1) })
end

-- potentially match a token
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

-- require a match of a token
local function consume(lexer, ttype, what)
    local last = lexer.get(0)
    local token = lexer.adv()
    if ttype ~= T.Newline then
        while token.type == T.Newline do
            token = lexer.adv()
        end
    end
    if token.type ~= ttype then
        --print("prev", lexer.get(-1))
        err(lexer, last, error_t, "Expected '" .. T.strings[ttype] .. "'" .. (what or ""), nil, true)
    end
    return token
end

-- require a newline or semicolon to end a statement
local function consume_end(lexer)
    local token = lexer.adv()
    if token.type ~= T.Newline and token.type ~= T.Semi then
        --print("prev", lexer.get(-1))
        err(lexer, lexer.get(-1), error_t, "Expected newline or ';' after statement", nil, true)
    end
    return token
end

-- require a match of an operator token
local function consume_oper(lexer, oper, what)
    local token = lexer.adv()
    if token.type ~= T.Oper or token.oper ~= oper then
        err(lexer, lexer.get(-1), error_t, "Expected '" .. oper .. "'" .. (what or ""), nil, true)
    end
    return token
end

--[[ require a match of a type-valid name
local function typename(lexer, what)
    local token = lexer.adv()
    if token.type ~= T.Name or not token.typevalid then
        err("Expected valid type name" .. (what or ""))
    end
    return token
end--]]

return function(self)
    self.no_error[T.Comma] = true
    self.no_error[T.RPar] = true
    self.no_error[T.RBrace] = true
    self.no_error[T.RSQB] = true
    self.no_error[T.Else] = true
    self.no_error[T.Semi] = true

    -- literal tokens are used as-is in the syntax tree
    self:def_pre(T.Nil, identity)
    self:def_pre(T.True, identity)
    self:def_pre(T.False, identity)
    self:def_pre(T.Number, identity)
    self:def_pre(T.Name, identity)
    self:def_pre(T.String, identity)

    -- unary operators
    self:def_pre("-", prefix)
    self:def_pre("#", prefix)
    self:def_pre("!", prefix)

    -- binary operators
    self:def_post("||", P.Or, binop_left, true)
    self:def_post("&&", P.And, binop_left, true)
    self:def_post("==", P.Equality, binop_left)
    self:def_post("!=", P.Equality, binop_left)
    self:def_post("<", P.Comparison, binop_left)
    self:def_post(">", P.Comparison, binop_left)
    self:def_post("<=", P.Comparison, binop_left)
    self:def_post(">=", P.Comparison, binop_left)
    self:def_post("??", P.Coalescing, binop_left, true)
    self:def_post("..", P.Concat, binop_right, true)
    self:def_post("+", P.Addition, binop_left, true)
    self:def_post("-", P.Addition, binop_left, true)
    self:def_post("/", P.Multiplication, binop_left, true)
    self:def_post("*", P.Multiplication, binop_left, true)
    self:def_post("%", P.Multiplication, binop_left, true)
    self:def_post("^", P.Power, binop_right, true)

    -- conditional expression
    -- if (cond) true_branch else false_branch
    self:def_pre(T.If, function(parser, _token)
        local s = expr({ ET.Conditional })
        consume(parser.lexer, T.LPar, " before conditional expression condition")
        s.cond = parser:expr()
        consume(parser.lexer, T.RPar, " after conditional expression condition")
        s.true_branch = parser:expr()
        consume(parser.lexer, T.Else, " in conditional expression")
        s.false_branch = parser:expr()
        return s
    end)

    -- assignment
    local _assignables = enum({ ET.ExprIndex, ET.NameIndex, T.Name })
    self:def_post("=", P.Assignment, function(parser, token, left, prec)
        local t = left.type or left[1].type or left[1]
        if not _assignables[t == T.Oper and left[1].oper or t] then
            -- TODO: err
            print(left, t)
            err(parser, token, error_t, "Invalid assignment target")
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

    -- call arguments
    local function call(parser)
        local i = 1
        local args = {}
        if not match(parser.lexer, T.RPar) then
            repeat
                args[i] = parser:expr()
                i = i + 1
            until not match(parser.lexer, T.Comma)
            consume(parser.lexer, T.RPar, " after function call")
        end
        if match(parser.lexer, T.LBrace) then
            args[i] = block_lambda(parser)
        end

        return args
    end

    -- name indexing - a.b
    self:def_post(".", P.Primary, function(parser, token, left)
        return expr({
            ET.NameIndex,
            token,
            from=left,
            index=consume(parser.lexer, T.Name)
        })
    end)

    -- method call - a:b(c)
    self:def_post(":", P.Primary, function(parser, token, left)
        local name = consume(parser.lexer, T.Name)
        local args
        if match(parser.lexer, T.LBrace) then
            args = { block_lambda(parser) }
        else
            consume(parser.lexer, T.LPar, " for method call")
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

    -- call - a(b, c)
    self:def_post(T.LPar, P.Primary, function(parser, _token, left)
        return expr({
            ET.Call,
            target=left,
            args=call(parser)
        })
    end)

    -- trailing lambda call - a {}
    self:def_post(T.LBrace, P.Primary, function(parser, _token, left)
        -- TODO: restrict allowed expression types
        return expr({
            ET.Call,
            target=left,
            args={ block_lambda(parser) }
        })
    end, false, true)

    -- pipeline call - b |> a(c)
    self:def_post("|>", P.Pipeline, function(parser, _token, left)
        local e = parser:expr(P.Primary - 1)
        local t = e[1]

        if t ~= ET.Call and t ~= ET.Namecall then
            err("Invalid pipeline call target.")
        end

        -- shift all args
        local args = e.args
        for i=#args,1,-1 do
            args[i+1] = args[i]
        end
        -- add left side of the operator as the first argument
        args[1] = left
        return e
    end)
    -- TODO: pipeline assignment

    -- indexing by expression
    self:def_post(T.LSQB, P.Primary, function(parser, token, left)
        return expr({
            ET.ExprIndex,
            token,
            from=left,
            index=parser:expr(),
            consume(parser.lexer, T.RSQB, " in indexing expression")
        })
    end)

    -- grouping parentheses
    self:def_pre(T.LPar, function(parser, _token)
        local e = parser:expr()
        consume(parser.lexer, T.RPar, " to end grouping parentheses")
        return e
    end)

    -- variable declaration
    self:def_stmt(T.Let, function(parser, _token, no_end)
        -- TODO: function declaration
        local name = consume(parser.lexer, T.Name)
        consume_oper(parser.lexer, "=", " in declaration")
        local value = parser:expr()
        if not no_end then
            consume_end(parser.lexer)
        end
        return expr({ ET.Declare, name=name, value=value })
    end)

    -- block "statement"
    self:def_stmt(T.LBrace, function(parser, _token)
        local stmts = {}
        local i = 1
        while not match(parser.lexer, T.RBrace) do
            stmts[i] = parser:stmt()
            i = i + 1
        end
        return expr({ ET.Block, stmts=stmts })
    end)

    self:def_stmt(T.Return, function(parser, _token, no_end)
        local e = parser:expr()
        if not no_end then
            consume_end(parser.lexer)
        end
        return expr({ ET.Return, e })
    end)

    self:def_stmt(T.Do, function(parser, _token)
        return parser:stmt(true)
    end)

    self:def_stmt(T.If, function(parser, _token, no_end)
        local s = expr({ ET.If })
        consume(parser.lexer, T.LPar, " before 'if' condition")
        s.cond = parser:expr()
        consume(parser.lexer, T.RPar, " after 'if' condition")
        s.true_branch = parser:stmt(true, true)
        if match(parser.lexer, T.Else) then
            s.false_branch = parser:stmt(true, true)
        end
        if not no_end then
            consume_end(parser.lexer)
        end
        return s
    end)

    self:def_stmt(T.While, function(parser, _token, _no_end)
        local s = expr({ ET.While })
        consume(parser.lexer, T.LPar, " before 'while' condition")
        s.cond = parser:expr()
        consume(parser.lexer, T.RPar, " after 'while' condition")
        s.body = parser:stmt(true)
        return s
    end)

    -- for statement
    self:def_stmt(T.For, function(parser, _token)
        consume(parser.lexer, T.LPar)
        local name = consume(parser.lexer, T.Name)
        local comma = match(parser.lexer, T.Comma)

        -- parse the k,v part of "for (k,v in ...)"
        -- numeric for-loops only ever have a single variable,
        -- so going through this path means the loop is an interator for loop
        if comma then
            name = { name }
            repeat
                name[#name+1] = consume(parser.lexer, T.Name)
            until not match(parser.lexer, T.Comma)
            consume(parser.lexer, T.In, " in iterator 'for' loop")
        end

        if comma or match(parser.lexer, T.In) then
            -- parse iterator for loops
            local s = expr({ ET.ForIter })
            if not comma then
                name = { name }
            end

            s.names = name
            s.iter = parser:expr() -- TODO: that thing
            consume(parser.lexer, T.RPar)

            s.body = parser:stmt(true)
            return s
        else
            -- parse numeric for loops
            local s = expr({ ET.ForNum })
            s.name = name
            consume_oper(parser.lexer, "=")
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

    self:def_stmt(T.Loop, function(parser, _token)
        return expr({ ET.Loop, parser:stmt(true) })
    end)

    self:def_stmt(T.Break, function(_parser, _token)
        return expr({ ET.Break })
    end)
end

local ast = require("parse.ast")
local lexer = require("parse.lexer")
local stream = require("parse.stream")
local utils = require("common.utils")

local enum = utils.enum
local T = lexer.token_types
local ET = ast.expr_types
local expr = ast.expr

local DEBUG_ERRORS = true

local errors = require("common.errors")
local error_t = errors.types.syntax_error
local function error(o, ...)
    errors.at_token(o.source, ...)
    if DEBUG_ERRORS then
        print(debug.traceback("", 2))
    end
    os.exit(1)
end

local function quote(t)
    if type(t) == "string" then
        return "token '"..t.."'"
    elseif t.type == T.Oper then
        return "token '"..t.oper.."'"
    elseif t.type == T.Number then
        return "number"
    elseif t.type == T.String then
        return "string"
    elseif t.type == T.Name then
        return "name"
    elseif t.type == T.Newline then
        return "newline"
    elseif t.type == T.EOF then
        return "end of file"
    else
        return "token '"..T.strings[t.type].."'"
    end
end

local r_opers_arr = { "^", "#", "*", "%", "/", "+", "-", "<", "<=", ">", ">=", "|>", "->", "." }
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
        error(lexer, last, error_t, "Expected '" .. T.strings[ttype] .. "'" .. (what or ""), nil, true)
    end
    return token
end

-- require a newline or semicolon to end a statement
local function consume_end(lexer)
    local token = lexer.adv()
    if token.type ~= T.Newline and token.type ~= T.Semi then
        --print("prev", lexer.get(-1))
        error(lexer, lexer.get(-1), error_t, "Expected newline or ';' after statement", nil, true)
    end
    return token
end

-- require a match of an operator token
local function consume_oper(lexer, oper, what)
    local token = lexer.adv()
    if token.type ~= T.Oper or token.oper ~= oper then
        error(lexer, lexer.get(-1), error_t, "Expected '" .. oper .. "'" .. (what or ""), nil, true)
    end
    return token
end

-- require a match of a type-valid name
local function typename(lexer, what)
    local token = lexer.adv()
    if token.type ~= T.Name or not token.typevalid then
        error("Expected valid type name" .. (what or ""))
    end
    return token
end

-- this is a pratt parser
local parser = {}
local parser_meta = { __index=parser }
function new_parser(source)
    local self = setmetatable({
        source = source,
        lexer = stream(lexer.get_next, source),
        stmts = {}, pre = {}, post = {},
        no_error = {}
    }, parser_meta)

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

    -- conditional expression
    -- if (cond) true_branch else false_branch
    self:def_pre(T.If, function(parser, token)
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
            -- TODO: error
            print(left, t)
            error(parser, token, error_t, "Invalid assignment target")
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
    self:def_post(T.LPar, P.Primary, function(parser, token, left)
        return expr({
            ET.Call,
            target=left,
            args=call(parser)
        })
    end)

    -- trailing lambda call - a {}
    self:def_post(T.LBrace, P.Primary, function(parser, token, left)
        -- TODO: restrict allowed expression types
        return expr({
            ET.Call,
            target=left,
            args={ block_lambda(parser) }
        })
    end, false, true)

    -- pipeline call - b |> a(c)
    self:def_post("|>", P.Pipeline, function(parser, token, left)
        local e = parser:expr(P.Primary - 1)
        local t = e[1]

        if t ~= ET.Call and t ~= ET.Namecall then
            error("Invalid pipeline call target.")
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
    self:def_pre(T.LPar, function(parser, token)
        local e = parser:expr()
        consume(parser.lexer, T.RPar, " to end grouping parentheses")
        return e
    end)

    -- variable declaration
    self:def_stmt(T.Let, function(parser, token, no_end)
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

    self:def_stmt(T.While, function(parser, token, no_end)
        local s = expr({ ET.While })
        consume(parser.lexer, T.LPar, " before 'while' condition")
        s.cond = parser:expr()
        consume(parser.lexer, T.RPar, " after 'while' condition")
        s.body = parser:stmt(true)
        return s
    end)

    -- for statement
    self:def_stmt(T.For, function(parser, token)
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
            if not comma then name = { name } end
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

    self:def_stmt(T.Loop, function(parser, token, no_end)
        return expr({ ET.Loop, parser:stmt(true) })
    end)

    self:def_stmt(T.Break, function(parser, token)
        return expr({ ET.Break })
    end)

    return self
end

-- most binary operators are not allowed as expressions,
-- but assignment is an operator and user-defined operators might
-- have intentional side effects so we can't just ban all operators
-- this table is used to determine the operators and expression types to ban
local disallowed_binops = enum({
    "#", "!", "||", "&&", "==", "!=", "<", ">", "<=", ">=", "??", "..", "+", "-", "/", "*", "%",
    ET.Lambda, ET.NameIndex, ET.ExprIndex
}, false, true)
-- parse a statement
function parser:stmt(allow_block, no_end)
    local token = self.lexer.adv()

    -- skip newlines and semicolons
    while token.type == T.Newline or token.type == T.Semi do
        token = self.lexer.adv()
    end

    local func = self.stmts[token.type]

    local stmt
    if func and (allow_block or token.type ~= T.LBrace) then
        stmt = func(self, token, no_end)
    else
        self.lexer.recede(1)
        local e = self:expr()
        -- restrict types of expressions allowed as statements
        -- e.type checks if the expression is a single token (those defined with identity - literals),
        -- all tokens have a .type field, and other syntax tree nodes are arrays
        if e.type or disallowed_binops[e[1].type == T.Oper and e[1].oper or e[1]] then
            if e.type then
                token = e
            elseif e[1].type then
                token = e[1]
            end
            error(self, token, error_t, ("%s expression is not allowed as a statement"):format(e.type or e[1]))
        end

        stmt = expr({ ET.Expression, e })
        if not no_end then
            consume_end(self.lexer)
        end
    end
    return stmt
end

-- parse an expression
function parser:expr(prec)
    -- the default maximum precedence
    if not prec then prec = P.Assignment-1 end

    -- get next token, skipping newlines
    local token = self.lexer.adv()
    while token.type == T.Newline do
        token = self.lexer.adv()
    end

    -- get prefix parse rule
    local idx = token.type
    if token.type == T.Oper then
        idx = token.oper
    end
    local func = self.pre[idx]
    -- if no rule is found, error with unexpected token
    if not func then
        error(self, token, error_t, "Unexpected "..quote(token))
    end

    local left = func(self, token, prec)
    repeat
        local i = 1
        -- get next token, skipping newlines
        token = self.lexer.get(i)
        while token.type == T.Newline do
            i = i + 1
            token = self.lexer.get(i)
        end
        -- stop parsing if end of file
        if token.type == T.EOF then break end

        -- get parse rule
        idx = token.type
        if token.type == T.Oper then
            idx = token.oper
        end
        local rule = self.post[idx]

        if not rule then
            -- prefer erroring for unexpected token here, even if not required
            if not self.no_error[idx] and not self.pre[idx] and not self.stmts[idx] then
                -- without this, invalid inputs might return to a
                -- lower level and then error there for obscure reasons
                -- example: "a(b ## c)" (imagine ## is undefined here),
                -- the function breaks, returns to call and errors with "Expected RPar",
                -- the same input outside of a call returns to stmt and then error with
                -- an even more obscure error, "Name expression is not allowed as a statement",
                -- whereas clearly the real problem is the invalid operator
                error(self, token, error_t, "Unexpected "..quote(token))
            end

            -- if the parser could be expecting that token somewhere else,
            -- return to a lower level without erroring
            break
        end

        if rule.prec+0 <= prec+0 or (i > 1 and not rule.skipnl) then break end

        -- advance the lexer to the token we are using
        self.lexer.adv(i)
        -- actually parse
        left = rule.func(self, token, left, rule.prec)
    until false

    return left
end

-- define statement parse rule
function parser:def_stmt(tok, func)
    self.stmts[tok] = func
end

-- define prefix parse rule
function parser:def_pre(tok, func)
    self.pre[tok] = func
end

-- define postfix parse rule
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

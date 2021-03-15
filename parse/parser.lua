local ast = require("parse.ast")
local ilexer = require("parse.lexer")
local grammar = require("parse.grammar")
local stream = require("common.stream")
local utils = require("common.utils")

local enum = utils.enum
local T = ilexer.token_types
local ET = ast.expr_types
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

-- require a newline or semicolon to end a statement
local function consume_end(lexer)
    local token = lexer.adv()
    if token.type ~= T.Newline and token.type ~= T.Semi then
        err(lexer, lexer.get(-1), error_t, "Expected newline or ';' after statement", nil, true)
    end
    return token
end

local P = require("parse.precedence")

local parser = {}
local parser_meta = { __index=parser }
function new_parser(source)
    local self = setmetatable({
        source = source,
        -- TODO: only enable pregen for benchmarking/debug
        lexer = stream(ilexer.get_next, source, true),
        stmts = {}, pre = {}, post = {},
        no_error = {}
    }, parser_meta)

    grammar(self)

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
            err(self, token, error_t, ("%s expression is not allowed as a statement"):format(e.type or e[1]))
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
    -- default to parsing maximum prec
    if not prec then
        prec = P.Assignment - 1
    end

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
        err(self, token, error_t, "Unexpected "..quote(token))
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
                err(self, token, error_t, "Unexpected "..quote(token))
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
        self.post[tok] = { prec=P.Assignment, func=function(parser_, token_, left_, prec_)
            return expr({ ET.OperAssign, token_, left_, parser_:expr(prec_ - 1) })
        end }
    end
end

return { new_parser=new_parser, parser=parser }

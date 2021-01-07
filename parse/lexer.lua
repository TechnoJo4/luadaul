local T = require("parse.token")
local token = require("parse.ast").token
local utils = require("parse.utils")
local chrtbl, enum = utils.chrtbl, utils.enum

local space_chars = " \t\r\n"
local spaces_arr = chrtbl(space_chars)
local spaces = enum(spaces_arr)

local keywords = {
    ["of"] = T.Of,
    ["let"] = T.Let,
    ["type"] = T.Type,
    ["export"] = T.Export,
    ["return"] = T.Return,
    ["true"] = T.True,
    ["false"] = T.False,
    ["nil"] = T.Nil
}

local symbols = {
    ["\\"] = T.Backslash,
    [","] = T.Comma, [";"] = T.Semi,
    ["{"] = T.LBrace, ["}"] = T.RBrace,
    ["("] = T.LPar, [")"] = T.RPar,
    ["["] = T.LSQB, ["]"] = T.RSQB,
}

local oper = enum(chrtbl("+-*/%<=>#.:^?!$@|&~"))

local typename_start_str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
local name_start_str = typename_start_str.."abcdefghijklmnopqrstuvwxyz_"
local typename_str = name_start_str.."'"
local name_str = name_start_str.."0123456789'"

local typename_start = enum(chrtbl(typename_start_str))
local name_start = enum(chrtbl(name_start_str))
local typename = enum(chrtbl(typename_str))
local name = enum(chrtbl(name_str))

local function get_next(source, pos, line, column)
    if not pos then
        pos = 1
        line = 1
        column = 1
    end

    local len = #source
    local c = source:sub(pos, pos)
    while pos <= len and spaces[c] do
        pos = pos + 1
        if c == "\n" then
            line = line + 1
            column = 1
        else
            column = column + 1
        end
        c = source:sub(pos, pos)
    end
    if pos > len then
        return token({ type=T.EOF, pos=pos, len=0, line=line, column=column })
    end

    local t = symbols[c]
    if t then
        return token({ type=t, pos=pos, len=1, line=line, column=column }), pos+1, line, column+1
    end

    if name_start[c] then
        local start = pos
        local startl = line
        local startc = column
        local typevalid = typename_start[c] and true

        repeat
            pos = pos + 1
            column = column + 1
            c = source:sub(pos, pos)
            typevalid = typevalid and (not name[c] or typename[c])
        until not name[c]

        local str = source:sub(start, pos - 1)
        if keywords[str] then
            return token({
                type = keywords[str],
                pos = start, len = pos - start,
                line = startl, column = startc
            }), pos, line, column
        end

        return token({
            type = T.Name,
            name = str,
            typevalid = typevalid,
            pos = start, len = pos - start,
            line = startl, column = startc
        }), pos, line, column
    end

    local function lex_rep(enum, ttype, key, action, pre)
        if enum[c] then
            local start = pos
            local startl = line
            local startc = column

            repeat
                pos = pos + 1
                column = column + 1
                c = source:sub(pos, pos)
            until not enum[c]

            local str = source:sub(start, pos - 1)
            if action then str = action(pre and pre..str or str) end
            return token({
                type = ttype, [key] = str,
                pos = start, len = pos - start,
                line = startl, column = startc
            }), pos, line, column
        end
        return
    end

    local ret = { lex_rep(oper, T.Oper, "oper") }
    if ret[1] then
        if ret[1].oper == "." then
            pos, column, c = ret[1].pos, ret[1].column, "."
        else
            return unpack(ret)
        end
    end

    local digit = enum(chrtbl("0123456789."))
    if c == '0' then
        pos = pos + 1
        column = column + 1
        c = source:sub(pos, pos)
        if c == 'x' then
            pos = pos + 1
            column = column + 1
            c = source:sub(pos, pos)
            ret = { lex_rep(enum(chrtbl("0123456789abcdefABCDEF")), T.Number, "num", tonumber, "0x") }
            if ret[1] then
                return unpack(ret)
            else
                p(ret)
                -- TODO: error
            end
        elseif c == 'b' then
            pos = pos + 1
            column = column + 1
            c = source:sub(pos, pos)
            ret = { lex_rep(enum(chrtbl("01")), T.Number, "num", function()
                -- TODO: parse binary
            end) }
            if ret[1] then
                return unpack(ret)
            else
                -- TODO: error
            end
        elseif not digit[c] then
            return token({
                type = T.Number, num = 0,
                pos = pos-1, len = 1,
                line = line, column = column-1
            }), pos, line, column
        end
    end

    if digit[c] or c == '.' then
        local start = pos
        local startc = column
        local dot = c == '.'

        repeat
            pos = pos + 1
            column = column + 1
            c = source:sub(pos, pos)
            if c == '.' then
                if dot then
                    -- TODO: error
                else
                    dot = true
                end
            end
        until not digit[c]

        if start + 1 == pos and dot then
            -- the only character is a dot,
            -- use ret from oper lex_rep
            return unpack(ret)
        end

        return token({
            type = T.Number, num = tonumber(source:sub(start, pos - 1)),
            pos = start, len = pos - start,
            line = line, column = startc
        }), pos, line, column
    end

    -- TODO: error
    p(source:sub(pos-1))
    error("what the fuck")
end

local function get_all(source)
    local tbl = {}
    local i = 1
    local tok, pos, line, column = get_next(source)

    repeat
        tbl[i] = tok
        tok, pos, line, column = get_next(source, pos, line, column)
        i = i + 1
    until not pos

    return tbl
end

return { get_next=get_next, get_all=get_all, token_types=T }

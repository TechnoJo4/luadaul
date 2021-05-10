local T = require("parse/token")
local token = require("parse/ast").token

local errors = require("common/errors")
local error_t = errors.types.syntax_error
local function err(...)
    errors.at_pos(...)
    os.exit(1)
end

local sbyte = string.byte

local keywords = {
    ["of"] = T.Of,
    ["let"] = T.Let,
    ["new"] = T.New,
    ["typedef"] = T.Type,
    ["return"] = T.Return,
    ["if"] = T.If,
    ["else"] = T.Else,
    ["do"] = T.Do,
    ["while"] = T.While,
    ["loop"] = T.Loop,
    ["for"] = T.For,
    ["in"] = T.In,
    ["break"] = T.Break,
    ["true"] = T.True,
    ["false"] = T.False,
    ["nil"] = T.Nil
}

local symbols = {
    [sbyte("\\")] = T.Backslash,
    [sbyte(",")] = T.Comma, [sbyte(";")] = T.Semi,
    [sbyte("{")] = T.LBrace, [sbyte("}")] = T.RBrace,
    [sbyte("(")] = T.LPar, [sbyte(")")] = T.RPar,
    [sbyte("[")] = T.LSQB, [sbyte("]")] = T.RSQB,
}

local escapes = {
    [sbyte("\\")] = "\\",
    [sbyte('"')] = '"',
    [sbyte("'")] = "'",
    [sbyte("a")] = "\a",
    [sbyte("b")] = "\b",
    [sbyte("f")] = "\f",
    [sbyte("n")] = "\n",
    [sbyte("r")] = "\r",
    [sbyte("t")] = "\t",
    [sbyte("v")] = "\v",
}

-- TODO: keyword identifiers (e.g. `in`)
--local grave = sbyte("`")

local single_quote = sbyte("'")
local double_quote = sbyte('"')

local function oper(c)
    -- +-*/%<=>#.:^?!$@|&~
    -- refer to ascii table
    -- TODO: precompute sbyte
    return c == sbyte("!")
        or c >= sbyte("#") and c <= sbyte("&") -- # $ % &
        or c >= sbyte("-") and c <= sbyte("/") -- - . /
        or c >= sbyte("<") and c <= sbyte("@") -- < = > ? @
        or c == sbyte("*") or c == sbyte("+")
        or c == sbyte(":")
        or c == sbyte("^") or c == sbyte("|") or c == sbyte("~")
end

local zero, nine = sbyte("0"), sbyte("9")
local function digit_nodot(c)
    return c >= zero and c <= nine
end

local dot = sbyte(".")
local function digit(c)
    return digit_nodot(c) or c == dot
end

local A, Z = sbyte("A"), sbyte("Z")
local function typename_start(c)
    return c >= A and c <= Z
end

local a, z, underscore = sbyte("a"), sbyte("z"), sbyte("_")
local function name_start(c)
    return typename_start(c) or (c >= a and c <= z) or c == underscore
end

local function typename(c)
    return name_start(c) or c == single_quote
end

local function name(c)
    return typename(c) or digit_nodot(c)
end

local newline, _r, tab, space = sbyte("\n"), sbyte("\r"), sbyte("\t"), sbyte(" ")
local function spaces(c)
    return c == newline or c == _r or c == tab or c == space
end

local function digit_hex(c)
    return digit_nodot(c)
end

local function lex_rep(source, pos, line, column, check, ttype, key, action, pre)
    local c = sbyte(source, pos)
    if check(c) then
        local start = pos
        local startl = line
        local startc = column

        repeat
            pos = pos + 1
            column = column + 1
            c = sbyte(source, pos)
        until not check(c)

        local str = source:sub(start, pos - 1)

        if action then
            if pre then
                str = action(pre .. str)
            else
                str = action(str)
            end
        end

        if not ttype then
            return str, pos, startl, column
        end

        return token({
            type = ttype, [key] = str,
            pos = start, len = pos - start,
            line = startl, column = startc
        }), pos, line, column
    end
    return
end

local function get_next(source, pos, line, column)
    if not pos then
        pos = 1
        line = 1
        column = 1
    end

    local len = #source
    local c = sbyte(source, pos)

    -- skip whitespace
    while pos <= len and spaces(c) do
        pos = pos + 1
        if c == newline then
            line = line + 1
            column = 1
            return token({ type = T.Newline, pos = pos-1, line = line, column = 0 }), pos, line, column
        elseif c == tab then
            column = column + 4
        else
            column = column + 1
        end
        c = sbyte(source, pos)
    end

    -- comments
    if c == sbyte("-") and sbyte(source, pos+1) == sbyte("-") then
        while pos <= len do
            pos = pos + 1
            if c == newline or (c == _r and sbyte(source, pos) ~= newline) then
                line = line + 1
                column = 1
                return token({ type = T.Newline, pos = pos-1, line = line, column = 0 }), pos, line, column
            end
            c = sbyte(source, pos)
        end
    end

    if pos > len then
        return token({ type=T.EOF, pos=pos, len=1, line=line, column=column })
    end

    if c == single_quote or c == double_quote then
        local start = pos
        local startcol = column
        local startchar = c
        local data = ""

        local escape = false
        repeat
            pos = pos + 1
            column = column + 1
            c = sbyte(source, pos)
            if pos > len then
                err(source, pos, line, column, error_t, "Unfinished string")
            end

            if escape then
                if escapes[c] then
                    data = data .. escapes[c]
                elseif digit_nodot(c) then
                    local n, pos_, line_, column_ = lex_rep(source, pos, line, column, digit_nodot, nil, nil, tonumber)
                    if n > 255 then
                        err(source, pos, line, column, error_t, "Invalid escape")
                    end
                    data = data .. string.char(n)
                    pos, line, column = pos_ - 1, line_, column_ - 1
                elseif c == 'x' then
                    local s = source:sub(pos+1, pos+2)
                    if #s ~= 2 then
                        err(source, pos, line, column, error_t, "Unfinished string")
                    end
                    local n = tonumber("0x"..s)
                    if not n or n > 255 then
                        err(source, pos, line, column, error_t, "Invalid escape")
                    end
                    data = data .. string.char(n)
                    pos = pos + 2
                else
                    err(source, pos, line, column, error_t, "Invalid escape")
                end
                escape = false
            elseif c == sbyte("\\") then
                escape = true
            elseif c == startchar then
                break -- returns outside of loop
            elseif c == sbyte("\r") or c == sbyte("\n") then
                err(source, pos, line, column, error_t, "Unfinished string")
            else
                data = data .. string.char(c)
            end
        until false

        pos = pos + 1
        return token({ type=T.String, data=data, pos=start, len=pos-start, line=line, column=startcol }), pos, line, column+1
    end

    local t = symbols[c]
    if t then
        return token({ type=t, pos=pos, len=1, line=line, column=column }), pos+1, line, column+1
    end

    if name_start(c) then
        local start = pos
        local startl = line
        local startc = column
        local typevalid = typename_start(c)

        while true do
            pos = pos + 1
            column = column + 1
            c = sbyte(source, pos)
            if not name(c) then break end
            typevalid = typevalid and typename(c)
        end

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

    local ret = { lex_rep(source, pos, line, column, oper, T.Oper, "oper") }
    if ret[1] then
        if ret[1].oper == "." then
            pos, column, c = ret[1].pos, ret[1].column, dot
        else
            return unpack(ret)
        end
    end

    if c == zero then
        pos = pos + 1
        column = column + 1
        c = sbyte(source, pos)
        if c == sbyte('x') then
            pos = pos + 1
            column = column + 1
            c = sbyte(source, pos)
            ret = { lex_rep(source, pos, line, column, digit_hex, T.Number, "num", tonumber, "0x") }
            if ret[1] then
                return unpack(ret)
            else
                err(source, ret[2], ret[3], ret[4], error_t, "Invalid hexadecimal literal")
            end
        elseif not digit(c) then
            return token({
                type = T.Number, num = 0,
                pos = pos-1, len = 1,
                line = line, column = column-1
            }), pos, line, column
        end
    end

    if digit(c) then
        local start = pos
        local startc = column
        local hasdot = c == dot

        repeat
            pos = pos + 1
            column = column + 1
            c = sbyte(source, pos)
            if c == dot then
                if hasdot then
                    err(source, pos, line, column, error_t, "Invalid number literal (more than one '.')")
                else
                    hasdot = true
                end
            end
        until not digit(c)

        if start + 1 == pos and hasdot then
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

    err(source, pos, line, column, error_t, "Unexpected character '"..sbyte(source, pos).."'")
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

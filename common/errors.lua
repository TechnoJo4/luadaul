--# selene: allow(multiple_statements)

local types = require("common.utils").enum({
    "syntax_error",
    "compile_error",
    "warning"
}, true, true)

local function getline(file, pos)
    -- find next newline
    local e = (file:find("[\r\n]", pos) or (#file+1)) - 1
    -- find last newline
    local s = 1
    for i=pos,1,-1 do
        local c = file:sub(i, i)
        if c == "\r" or c == "\n" then
            s = i + 1
            break
        end
    end

    -- skip start whitespace
    for i=s,pos do
        local c = file:sub(i, i)
        if c == " " or c == "\t" then
            s = i + 1
        else break end
    end

    -- length of a tab in the terminal can vary,
    -- so we just replace them with spaces to avoid
    -- the caret not being aligned with the error
    -- in the future, use magic to make this respect
    -- some sort of tab length volume and align correct
    local line = file:sub(s, e):gsub("\t", " ")
    return line, pos - s
end

local red = "\27[31m"
local yellow = "\27[33m"
local reset = "\27[0m"

local type_strs = {
    [types.warning] = "Warning",
    [types.syntax_error] = "Syntax error",
    [types.compile_error] = "Compile error",
}
local function type_str(t)
    local str = t == types.warning and yellow or red
    str = str .. type_strs[t]
    return str
end

-- print error message with prefix indicating type
local function message(t, msg, at, filename)
    t = type_str(t)
    if at then t = t .. " at " .. at end
    if filename then t = t .. " in " .. filename end
    io.write(t .. ":" .. reset .. " " .. msg .. "\n")
end

-- print a line with caret(s) at column
local function printline(str, column, color, len)
    io.write("    " .. str .. "\n")
    io.write("    " .. (" "):rep(column) .. color .. ("^"):rep(len or 1) .. reset .. "\n")
end

-- print an error from a position (in the lexer)
local function at_pos(src, pos, line, col, t, msg, filename)
    message(t, msg, line .. ":" .. col, filename)

    local l, c = getline(src, pos)
    printline(l, c, t == types.warning and yellow or red)
end

-- print an error from a token (in the parser/compiler)
local function at_token(src, tok, t, msg, filename, after)
    message(t, msg, tok.line .. ":" .. (after and tok.column + tok.len or tok.column), filename)

    local l, c = getline(src, tok.pos)
    if after then
        printline(l, c + tok.len, t == types.warning and yellow or red, 1)
    else
        printline(l, c, t == types.warning and yellow or red, tok.len)
    end
end

return {
    at_pos=at_pos,
    at_token=at_token,
    message=message,
    types=types
}

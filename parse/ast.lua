local T = require("parse.token")
local utils = require("parse.utils")
local enum = utils.enum

local token_meta = {
    __tostring = function(self)
        local s = " "
        if self.type == T.Oper then
            s = self.oper
        elseif self.type == T.String then
            s = self.data
        elseif self.type == T.Number then
            s = tostring(self.num)
        elseif self.type == T.Name then
            s = self.name
            if self.typevalid then
                s = s .. " (type-valid)"
            end
        elseif self.type == T.Error then
            s = self.error
        end
        if s ~= " " then s = " " .. s .. " " end
        return ("%s%s@ L%dC%d"):format(T[self.type], s, self.line, self.column)
    end
}
local function token(tbl)
    return setmetatable(tbl, token_meta)
end

local ET = enum({
    "Call",
    "Namecall",
    "NameIndex",
    "ExprIndex",
    "Return",
    "Expression",
    "Declare",
    "Block",
    "If",
    "While",
    "Loop",
    "Break",
}, true)

local expr_tostring
local function tostr(v, lvl)
    if type(v) == "table" and v.__expr then
        return expr_tostring(v, lvl+1)
    else
        return tostring(v)
    end
end

local ET_tostring = {
    [ET.Call] = function(expr, lvl)
        local args = {}
        for k,v in pairs(expr.args) do
            args[k] = (" "):rep(lvl) .. tostr(v, lvl)
        end

        return {
            "Call",
            "target="..tostr(expr.target, lvl),
            "args={\n"..table.concat(args, ",\n").."\n"..(" "):rep(lvl-1).."}"
        }
    end,
    [ET.Namecall] = function(expr, lvl)
        local args = {}
        for k,v in pairs(expr.args) do
            args[k] = (" "):rep(lvl) .. tostr(v, lvl)
        end

        return {
            "Namecall ("..tostring(expr[2])..")",
            "from="..tostr(expr.from, lvl),
            "target="..tostr(expr.target, lvl),
            "args={\n"..table.concat(args, ",\n").."\n"..(" "):rep(lvl-1).."}"
        }
    end,
    [ET.NameIndex] = function(expr, lvl)
        return {
            "NameIndex ("..tostr(expr[2], lvl)..")",
            "from="..tostr(expr.from, lvl),
            "index="..tostr(expr.index, lvl)
        }
    end,
    [ET.Return] = function(expr, lvl)
        return { "Return", tostr(expr[2], lvl) }
    end,
    [ET.Expression] = function(expr, lvl)
        return { "Expression", tostr(expr[2], lvl) }
    end,
    [ET.Declare] = function(expr, lvl)
        return {
            "Declare ("..tostr(expr[2], lvl)..")",
            "name="..tostr(expr.name, lvl),
            "value="..tostr(expr.value, lvl)
        }
    end,
    [ET.Block] = function(expr, lvl)
        local stmts = { "Block" }
        for k,v in pairs(expr.stmts) do
            stmts[k+1] = (" "):rep(k > 1 and lvl or 0) .. tostr(v, lvl)
        end

        return stmts
    end,
    [ET.If] = function(expr, lvl)
        return {
            "If",
            "cond="..tostr(expr.cond, lvl),
            "true="..tostr(expr.true_branch, lvl),
            expr.false_branch and "false="..tostr(expr.false_branch, lvl)
        }
    end,
    [ET.While] = function(expr, lvl)
        return {
            "While",
            "cond="..tostr(expr.cond, lvl),
            tostr(expr.branch, lvl)
        }
    end,
    [ET.Loop] = function(expr, lvl)
        return { "Loop", tostr(expr[2], lvl) }
    end,
    [ET.Break] = function(expr, lvl)
        return { "Break" }
    end,
}

expr_tostring = function(expr, lvl)
    if not lvl then lvl = 0 end
    local t
    if ET[expr[1]] then
        t = ET_tostring[expr[1]](expr, lvl)
    else
        t = {}
        for i,v in pairs(expr) do
            if v.__expr then
                t[i] = expr_tostring(v, lvl+1)
            else
                t[i] = tostring(v)
            end
        end
    end
    if #t > 1 then
        for i=2,#t do
            t[i] = (" "):rep(lvl) .. t[i]
        end
    end
    return "("..table.concat(t, "\n")..")"
end
local function expr(tbl)
    return setmetatable(tbl, { __tostring=expr_tostring, __index={ __expr=true } })
end

return { token=token, expr=expr, expr_types=ET }

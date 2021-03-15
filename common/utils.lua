-- table from characters in string
local function chrtbl(str)
    local tbl = {}
    for i=1,#str do
        tbl[i] = str:sub(i, i)
        i = i + 1
    end
    return tbl
end

-- bi: "bi-directional" 
-- by default, enum is a fun({ [TK]: TV }) -> { [TV]: TK_ },
-- bi=true makes the return type { [TV]: TK_, [TK_]: TV },
-- keeping the old indicies
-- nomt=true does not wrap the key in a table/metatable
local function enum(tbl, bi, nomt)
    local new = {}
    for k,v in pairs(tbl) do
        -- selene: allow(multiple_statements)
        local t = nomt and k or setmetatable({k}, {
            __lt = function(a, b)
                if type(a) == "table" then a = a[1] end
                if type(b) == "table" then b = b[1] end
                return a < b
            end,
            __le = function(a, b)
                if type(a) == "table" then a = a[1] end
                if type(b) == "table" then b = b[1] end
                return a <= b
            end,
            __add = function(a, b)
                if type(a) == "table" then a = a[1] end
                if type(b) == "table" then b = b[1] end
                return a + b
            end,
            __sub = function(a, b)
                if type(a) == "table" then a = a[1] end
                if type(b) == "table" then b = b[1] end
                return a - b
            end,
            __tostring = function(self)
                if type(self) == "table" then self = self[1] end
                return tbl[self]
            end
        })

        new[v] = t
        if bi then
            new[t] = v
        end
    end
    return new
end

return { chrtbl = chrtbl, enum = enum }

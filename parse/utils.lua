-- table from characters in string
local function chrtbl(str)
    local i = 1
    local tbl = {}
    for c in str:gmatch(".") do
        tbl[i] = c
        i = i + 1
    end
    return tbl
end

-- param bi is "bi-directional" 
-- by default, enum is a fun({ [TK]: TV }) -> { [TV]: TK },
-- bi=true makes the return type { [TV]: TK, [TK]: TV },
-- keeping the old indicies
local function enum(tbl, bi)
    local new = {}
    for k,v in pairs(tbl) do
        local t = setmetatable({k}, {
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

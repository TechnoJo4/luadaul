-- luvi main
local has_luvi, luvi = pcall(require, 'luvi')

local function bundle_load(file, ...)
    if has_luvi then
        return loadstring(luvi.bundle.readfile(file), "bundle:"..file)(...)
    else
        return loadfile(file)(...)
    end
end

bundle_load("common/luvit-loader.lua")

-- selene: allow(undefined_variable)
return bundle_load("init.lua", ...)

return {
    name = "luadaul",
    version = "0.1.0-beta0",
    description = "The daul compiler",
    tags = { "lua" },
    license = "MIT",
    author = { name = "TechnoJo4", email = "technojo4@gmail.com" },
    homepage = "https://github.com/TechnoJo4/luadaul",
    dependencies = {},
    files = {
        "LICENSE",
        "README.md",
        "package.lua",

        "luvit-loader.lua",

        "init.lua",
        "main.lua",
        "ir/**.lua",
        "emit/**.lua",
        "parse/**.lua",
        "common/**.lua",
    }
}

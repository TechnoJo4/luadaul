-- types of tokens
local enum = require("common.utils").enum({
    "Oper",
    "Number",
    "String",
    "Name",
    "Backslash",
    "Comma",
    "Dot",
    "Semi",
    "Newline",
    "LBrace",
    "RBrace",
    "LPar",
    "RPar",
    "LSQB",
    "RSQB",
    "Return",
    "Export",
    "Let",
    "Type",
    "Of",
    "Nil",
    "True",
    "False",
    "If",
    "Else",
    "Do",
    "While",
    "Loop",
    "For", "In",
    "Break",
    "EOF"
}, true)

enum.strings = {
    [enum.Backslash] = "\\",
    [enum.Comma] = ",",
    [enum.Dot] = ".",
    [enum.Semi] = ";",
    [enum.LBrace] = "{",
    [enum.RBrace] = "}",
    [enum.LPar] = "(",
    [enum.RPar] = ")",
    [enum.LSQB] = "[",
    [enum.RSQB] = "]",
    [enum.Return] = "return",
    [enum.Export] = "export",
    [enum.Let] = "let",
    [enum.Type] = "typedef",
    [enum.Of] = "of",
    [enum.Nil] = "nil",
    [enum.True] = "true",
    [enum.False] = "false",
    [enum.If] = "if",
    [enum.Else] = "else",
    [enum.Do] = "do",
    [enum.While] = "while",
    [enum.Loop] = "loop",
    [enum.For] = "for",
    [enum.In] = "in",
    [enum.Break] = "break"
}

return enum

return require("common.utils").enum({
    "NIL", "TRUE", "FALSE", "CONST",
    "GETTABLE", "SETTABLE",
    "GETLOCAL", "SETLOCAL",
    "GETUPVAL", "SETUPVAL",
    "GETGLOBAL", "SETGLOBAL",
    "ADD", "SUB",
    "MUL", "DIV", "MOD",
    "POW", "CONCAT",
    "UNM", "NOT", "LEN",
    "OR", "AND",
    "EQ", "NEQ",
    "LT", "LTEQ",
    "GT", "GTEQ",
    "CALL", "NAMECALL",
    "RETURN",
    "POP", "PUSH", "CLOSE",
    "CONDITIONAL",
    "IF", "BREAK", "LJ_LOOP",
    "LOOP", "NUMFOR", "ITERFOR",
    "CLOSURE",
    "NEWTABLE",
    "LAST"
}, true)

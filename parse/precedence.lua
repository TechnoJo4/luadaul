-- Precedence enum
-- TODO: find a smart way to make indicies sparse (or modify the enum at runtime) so
-- user-defined operator precedences can be between builtin operator precedences
return require("common/utils").enum({
    "Assignment",
    "Pipeline",
    "Or",
    "And",
    "Equality",
    "Comparison",
    "Coalescing",
    "Concat",
    "Addition",
    "Multiplication",
    "Unary",
    "Power",
    "Primary"
})

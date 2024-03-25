include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "common.mc" -- defines convenience functions e.g. printLn

mexpr
    use MExprPrettyPrint in
    let x = addi_ (var_ "x") (var_ "y") in
    printLn (expr2str x); dprintLn x

    -- run with boot eval test.mc (Debugging info is way better in boot than core compiler)
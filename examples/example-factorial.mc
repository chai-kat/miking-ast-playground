-- This is a small complete test
-- Will collect utests and stuff for StmtWhile here too

include "../imperativemexpr.mc"
include "../ast-builder.mc"
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"

-- boot eval examples/example1.mc

-- let const_ = use MExprAst in
--   lam ty. lam c.
--   TmConst {val = c, ty = ty, info = NoInfo ()}

lang Foo = ImperativeMExpr + MExprPrettyPrint end

-- show factorial with the following program:
-- def fact(x):
--     result = 1
--     while (x != 1) {
--         result *= x    
--         x -= 1
--     }
-- return result


-- should try something where you get a partially (un)symbolized ast?
-- e.g. factorial * multiplier (i.e. n! * (multiplier^n)):
-- let multiplier = 4 in
    -- def fact2(x):
    --     result = 1
    --     while (x != 1) {
    --         result *= (x * multiplier)
    --         x -= 1
    --     }
    -- return result

mexpr 
use Foo in
let imperative_ast = funcdecl_ 
    [
        -- nuvardecl_ (nameNoSym "y") (int_ 5),
        (nvardecl_ (nameNoSym "result") tyint_ (int_ 1)),
        (while_ (neqi_ (var_ "x") (int_ 1)) [
            (varassign_ (nameNoSym "result") (muli_ (var_ "result") (var_ "x"))), -- result = result*x
            (varassign_ (nameNoSym "x") (subi_ (var_ "x") (int_ 1)))  -- x = x-1
        ]),
        return_ (var_ "result")
    ]

    tyunknown_
    
    -- TODO: handle the case where params is empty!! 
    [
        -- how are these getting symbols when we explicitly say noSym?? Overwriting our environment symbols?
        param_ (nameNoSym "x") tyint_
    ]
 in

-- translateFuncDecl imperative_ast

dprintLn (translateFuncDecl imperative_ast);
printLn (expr2str (translateFuncDecl imperative_ast))

-- go with this, it works 
-- nlet_ (concat param.ident "1") param.ty (ref_ (var_ param.ident))

-- boot eval testimperative.mc
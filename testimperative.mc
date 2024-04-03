include "./imperativemexpr.mc"
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"


-- let const_ = use MExprAst in
--   lam ty. lam c.
--   TmConst {val = c, ty = ty, info = NoInfo ()}

mexpr 
use ImperativeMExpr in
let imperative_ast = TmFuncDecl {
    body = [
        StmtReturn {
            body = const_ tyint_ 0
        }
    ],
    ty = tyarrows_ [tyunit_, tyint_],
    params = [],
    ident = "main"
} in

dprintLn (translateFuncDecl imperative_ast)
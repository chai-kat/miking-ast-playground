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
            body = int_ 0
        }
    ],
    ty = tyarrows_ [tyunit_, tyint_],
    -- TODO: handle the case where params is empty!! 
    params = [
        {ty = tyunknown_, tyAnnot = tyunknown_, ident = (nameSym "x")}
        ]
    -- ident = nameSym "main"
    -- tyAnnot = tyunknown_
} in

dprintLn (translateFuncDecl imperative_ast)
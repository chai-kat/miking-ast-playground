include "../imperativemexpr.mc"
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"

-- boot eval examples/example1.mc

-- let const_ = use MExprAst in
--   lam ty. lam c.
--   TmConst {val = c, ty = ty, info = NoInfo ()}

lang Foo = ImperativeMExpr + MExprPrettyPrint end

mexpr 
use Foo in
let imperative_ast = TmFuncDecl {
    body = [
        -- StmtVarAssign {
        --     ident = (nameSym "x")
        --     value = addi_ (var_  "x") (int_ 5)
        -- }
        StmtReturn {
            body = var_ "x"
        }
    ],
    ty = tyarrows_ [tyunit_, tyint_],
    -- TODO: handle the case where params is empty!! 
    params = [
        {ty = tyunknown_, tyAnnot = tyunknown_, ident = (nameNoSym "hello")}
        ]
    -- ident = nameSym "main"
    -- tyAnnot = tyunknown_
} in

-- dprintLn (translateFuncDecl imperative_ast)
printLn (expr2str (translateFuncDecl imperative_ast))

-- go with this, it works 
-- nlet_ (concat param.ident "1") param.ty (ref_ (var_ param.ident))

-- boot eval testimperative.mc
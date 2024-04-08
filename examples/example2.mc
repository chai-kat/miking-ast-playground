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

mexpr 
use Foo in
let imperative_ast = funcdecl_ 
    [
        -- StmtVarAssign {
        --     ident = (nameSym "x")
        --     value = addi_ (var_  "x") (int_ 5)
        -- }
        return_ (var_ "x"),
        nuvardecl_ "woow" (int_ 0)
        
        -- const_ (tyint_) 5
        -- int_ 5 = const_ tyint_ 5

    ]

    tyunknown_
    
    -- TODO: handle the case where params is empty!! 
    [
        param_ (nameNoSym "hello") tyunknown_
    ]
    -- ident = nameSym "main"
    -- tyAnnot = tyunknown_
 in

dprintLn (translateFuncDecl imperative_ast)

-- go with this, it works 
-- nlet_ (concat param.ident "1") param.ty (ref_ (var_ param.ident))

-- boot eval testimperative.mc
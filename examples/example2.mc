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
        -- nuvardecl_ (nameNoSym "y") (int_ 5),
        return_ (nvar_ (nameNoSym "x"))
        --return_ (nvar_ (nameNoSym "y")),
        --return_ (nvar_ (nameNoSym "z"))
    ]

    tyunknown_
    
    -- TODO: handle the case where params is empty!! 
    [
        -- how are these getting symbols when we explicitly say noSym?? Overwriting our environment symbols?
        -- param_ (nameNoSym "hello") tyunknown_
    ]
 in

-- translateFuncDecl imperative_ast

dprintLn (translateFuncDecl imperative_ast);
printLn (expr2str (translateFuncDecl imperative_ast))

-- go with this, it works 
-- nlet_ (concat param.ident "1") param.ty (ref_ (var_ param.ident))

-- boot eval testimperative.mc
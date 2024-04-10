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

let factorial = lam x. 
  let rec fact = lam n. 
    if n == 0 then 1 else n * fact (n - 1) in
  fact x

let factorial = lam x:123. 
    let x:234 = ref x
    let result = ref 1 in
    while ((deref x:234) != 0) do
        modref x:234 = (deref x) - 1
        modref result = (deref result) * x
    endwhile
in

use Foo in
let imperative_ast = funcdecl_ 
    [
        return_ (var_ "x"),
        nuvardecl_ "woow" (int_ 0)
    ]

    tyunknown_
    
    -- TODO: handle the case where params is empty!! 
    [
        param_ (nameNoSym "hello") tyunknown_
    ]
 in

dprintLn (translateFuncDecl imperative_ast)

-- go with this, it works 
-- nlet_ (concat param.ident "1") param.ty (ref_ (var_ param.ident))

-- boot eval testimperative.mc
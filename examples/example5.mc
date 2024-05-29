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
let main () = 
  ref x = 5
  let return1 = lam x. x + 5
  let return2 = lam x. x
  match x with 5
    then return1 x
    else return2 x


let mexprwithfuncdecl = bind_ 
  nlet_ ("main") tyint_ 
    (funcdecl_ 
        [
            nuvardecl_ "x" (int_ 5),
            stmtmatch_ (var_ "x") 5 
              [return_ (app_ (var_ "return1") (var_ "x"))] -- if x = 5
              [return_ (app_ (var_ "return2") (var_ "x"))], -- else
            nuvardecl_ "y" (int_ 2)
        ]
        tyint_
        [
            param_ (nameNoSym "x") tyint_
        ]
    )
    (call_ main 5)

lam x. 
  let cont = lam env.
    match env.x with 5
      then env.x + 5
      else env.x
  cont env
  let y = ref 2 -- illegal in MExpr? 

use Foo in
let imperative_ast = funcdecl_ 
    [
        nuvardecl_ "x" (int_ 5)
        stmtmatch_ x 5 
        [return_ (var_ "x")] -- if x = 5
        [return_ (addi_ (var_ "x") (int_ 5))] -- else
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
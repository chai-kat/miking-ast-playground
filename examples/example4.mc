include "../imperativemexpr.mc"
include "../ast-builder.mc"
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "string.mc"

-- boot eval examples/example1.mc

-- let const_ = use MExprAst in
--   lam ty. lam c.
--   TmConst {val = c, ty = ty, info = NoInfo ()}

lang Foo = ImperativeMExpr + MExprPrettyPrint end

mexpr 
use Foo in
let ast = 
    --ulam_ "random" (
        (ulet_ "tmp") (var_ "x")

in

let env = {_symEnvEmpty with allowFree = true} in


let symbolizeAst = symbolizeExpr env ast in

let printAst = lam ast.
    match ast with TmLet {ident = letIdent, body = TmVar {ident = varIdent}} in
    printLn (join [
      "tmLet hash: ", int2string (sym2hash (optionGetOr _noSymbol (nameGetSym letIdent))), "\n",
      "tmVar hash: ", int2string (sym2hash (optionGetOr _noSymbol (nameGetSym varIdent)))
    ])
  in

dprintLn (ast);
printLn (expr2str (ast));

dprintLn (symbolizeAst);
printLn (expr2str (symbolizeAst));

printAst ast;
printAst symbolizeAst


-- go with this, it works 
-- nlet_ (concat param.ident "1") param.ty (ref_ (var_ param.ident))

-- boot eval testimperative.mc
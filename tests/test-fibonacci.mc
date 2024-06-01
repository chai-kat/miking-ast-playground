include "../imperativemexpr.mc"
include "../ast-builder.mc"
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"

lang Foo = ImperativeMExpr + MExprPrettyPrint end

mexpr 
use Foo in
let imperative_ast = funcdecl_ 
    [
        (nvardecl_ (nameNoSym "x1") tyunknown_ (int_ 0)),
        (nvardecl_ (nameNoSym "x2") tyunknown_ (int_ 1)),
        (nvardecl_ (nameNoSym "sum") tyunknown_ (int_ 0)),
        (nvardecl_ (nameNoSym "n") tyunknown_ (int_ 1)),
        (stmtmatch_ (lti_ (var_ "x") (int_ 2)) ptrue_
            [(return_ (var_ "x"))] 
            [(while_ (neqi_ (var_ "n") (var_ "x")) [
                (varassign_ (nameNoSym "sum") (addi_ (var_ "x1") (var_ "x2"))),
                (varassign_ (nameNoSym "x1") (var_ "x2")),
                (varassign_ (nameNoSym "x2") (var_ "sum")),
                (varassign_ (nameNoSym "n") (addi_ (var_ "n") (int_ 1)))
            ]),
            return_ (var_ "sum")])
    ]
    
    tyunknown_
    
    [
        param_ (nameNoSym "x") tyunknown_
    ]
in

-- translateFuncDecl imperative_ast

-- dprintLn (translateFuncDecl imperative_ast)
printLn (expr2str (translateFuncDecl imperative_ast))

-- go with this, it works 
-- nlet_ (concat param.ident "1") param.ty (ref_ (var_ param.ident))

-- boot eval testimperative.mc
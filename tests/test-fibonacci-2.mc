include "../imperativemexpr.mc"
include "../ast-builder.mc"
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"

lang ImperativeMExprTestingPrerequisites = 
ImperativeMExpr + MCoreCompileLang + MExprLowerNestedPatterns + MExprTypeCheck + BootParser + MExprPrettyPrint
end
mexpr
use ImperativeMExprTestingPrerequisites in

let imperative_ast = funcdecl_ 
    [
        (nvardecl_ (nameNoSym "a") tyunknown_ (int_ 0)),
        (nvardecl_ (nameNoSym "b") tyunknown_ (int_ 1)),
        (nvardecl_ (nameNoSym "sum") tyunknown_ (int_ 0)),
        (nvardecl_ (nameNoSym "n") tyunknown_ (int_ 1)),
        (stmtmatch_ (lti_ (var_ "x") (int_ 2)) ptrue_
            [(return_ (var_ "x"))] 
            []
        ),
        (while_ (neqi_ (var_ "n") (var_ "x")) [
                (varassign_ (nameNoSym "sum") (addi_ (var_ "a") (var_ "b"))),
                (varassign_ (nameNoSym "a") (var_ "b")),
                (varassign_ (nameNoSym "b") (var_ "sum")),
                (varassign_ (nameNoSym "n") (addi_ (var_ "n") (int_ 1)))
            ]),
        return_ (var_ "sum")
    ]
    
    tyunknown_
    
    [
        param_ (nameNoSym "x") tyunknown_
    ]
in

-- translateFuncDecl imperative_ast

-- dprintLn (translateFuncDecl imperative_ast)
-- printLn (expr2str (translateFuncDecl imperative_ast))

-- go with this, it works 
-- nlet_ (concat param.ident "1") param.ty (ref_ (var_ param.ident))

-- boot eval testimperative.mc

let program: String = strJoin "\n" [
      "include \"mexpr/pprint.mc\"",
      "mexpr",
      "let fib = ",
      expr2str (
        translateFuncDecl imperative_ast
      ),
      "in printLn (int2string (fib 6))"
    --   "in
    --   map (lam s. 
    --     print (int2string s);
    --     print \" \") (selectionSort [6, 1, 2, 5, 1, 3])"
      
    ] in
    printLn program;
    ()
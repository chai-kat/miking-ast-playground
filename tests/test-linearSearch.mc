include "mexpr/pprint.mc"
include "../ast-builder.mc"
lang ImperativeMExprTestingPrerequisites = 
ImperativeMExpr + MCoreCompileLang + MExprLowerNestedPatterns + MExprTypeCheck + BootParser
end
mexpr
use ImperativeMExprTestingPrerequisites in

let linearSearch = funcdecl_ 
    [
        (nvardecl_ (nameNoSym "index") tyunknown_ (int_ 0)),
        (while_ (lti_ (var_ "index") (length_ (var_ "arr"))) [
            (match (get_ (var_ "arr") (var_ "index")) (var_ "target") [
                (return_ (var_ "index"))
            ] []),
            (varassign_ (nameNoSym "index" ) (addi_ (var_ "i") (int_ 1)))
        ]),
        (return_ (int_ -1))
    ]

    tyunknown_

    [
        (param_ (nameNoSym "arr") tyunknown_),
        (param_ (nameNoSym "target") tyunknown_)
    ]
in

let program: String = strJoin "\n" [
      "include \"mexpr/pprint.mc\"",
      "mexpr",

    "let linearSearch = ",
    expr2str (
        translateFuncDecl linearSearch
    ),
      "in printLn (int2string (linearSearch [1, 3, 1, 4, 3] 3))"
    ] in
    printLn program;
    ()
include "mexpr/pprint.mc"
include "../ast-builder.mc"
lang ImperativeMExprTestingPrerequisites = 
ImperativeMExpr + MCoreCompileLang + MExprLowerNestedPatterns + MExprTypeCheck + BootParser
end
mexpr
use ImperativeMExprTestingPrerequisites in
let compare = funcdecl_
    [
        (stmtmatch_ (gti_ (var_ "a") (var_ "b")) ptrue_
            [return_ (var_ "a")]
            [return_ (var_ "b")]
        )
    ]

    tyunknown_

    [
        (param_ (nameNoSym "a") tyunknown_),
        (param_ (nameNoSym "b") tyunknown_)
    ]
in

let max = funcdecl_ 
    [
        (nvardecl_ (nameNoSym "result") tyunknown_ (int_ 0)),
        (nvardecl_ (nameNoSym "i") tyunknown_ (int_ 0)),
        (while_ (lti_ (var_ "i") (length_ (var_ "arr"))) [
            (varassign_ (nameNoSym "result") (appf2_ (var_ "compare") (var_ "result") (get_ (var_ "arr") (var_ "i")))),
            (varassign_ (nameNoSym "i" ) (addi_ (var_ "i") (int_ 1)))
        ]),
        return_ (var_ "result")
    ]

    tyunknown_

    [
        param_ (nameNoSym "arr") tyunknown_
    ]
in

let program: String = strJoin "\n" [
      "include \"mexpr/pprint.mc\"",
      "mexpr",

    "let compare = ",
    expr2str (
        translateFuncDecl compare
    ),
    "in",
      "let max = ",
      expr2str (
        translateFuncDecl max
      ),
      "in printLn (int2string (max [1, 3, 1, 4, 3]))"
    ] in
    printLn program;
    ()
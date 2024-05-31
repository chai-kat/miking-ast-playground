include "mexpr/ast.mc"
include "./imperativemexpr.mc"
include "mexpr/type.mc"

include "mexpr/symbolize.mc"
include "mexpr/pprint.mc"

include "bool.mc"
include "common.mc"
include "error.mc"
include "map.mc"
include "math.mc"
include "name.mc"
include "option.mc"
include "result.mc"
include "seq.mc"
include "set.mc"
include "sys.mc"
include "mexpr/boot-parser.mc"
include "mexpr/cmp.mc"
include "mexpr/info.mc"
include "mexpr/shallow-patterns.mc"
include "mexpr/type-check.mc"
include "ocaml/mcore.mc"

let stmtexpr_ = use ImperativeMExpr in
    lam e. StmtExpr {body = e}

let return_ = use ImperativeMExpr in
    lam e. StmtReturn {body = e}

-- named
-- ident: Name, value: Expr
let nvardecl_ = use ImperativeMExpr in
    lam ident. lam ty. lam value. StmtVarDecl {ident = ident, ty = ty, value = value}

-- named untyped
let nuvardecl_ = use ImperativeMExpr in
    lam ident. lam value. StmtVarDecl {ident = ident, ty = tyunknown_, value = value}

let varassign_ = use ImperativeMExpr in
    lam ident. lam e. StmtVarAssign {ident = ident, value = e}

let stmtmatch_ = use ImperativeMExpr in
    lam target_expr. lam pattern_pat. lam thn_stmtlist. lam else_stmtlist.
    StmtMatch {target = target_expr, pat = pattern_pat, thn = thn_stmtlist, els = else_stmtlist}

let while_ = use ImperativeMExpr in
    lam condexpr. lam stmtlist. StmtWhile {condition = condexpr, body = stmtlist}

let param_ = lam ident. lam ty. {ty = ty, tyAnnot = ty, ident = ident}

let funcdecl_ = use ImperativeMExpr in
    lam b. lam ty. lam params. TmFuncDecl {body = b, ty = ty, params = params}

let ufuncdecl_ = use ImperativeMExpr in
    lam b. lam params. TmFuncDecl {body = b, ty = tyunknown_, params = params}

let unitfuncdecl_ = use ImperativeMExpr in
    lam b. TmFuncDecl {body = b, ty = tyunit_, params = []}




lang ImperativeMExprTestingPrerequisites = 
ImperativeMExpr + MCoreCompileLang + MExprLowerNestedPatterns + MExprTypeCheck + BootParser
end
mexpr
use ImperativeMExprTestingPrerequisites in
let imperative_ast = funcdecl_ 
    [
        (nvardecl_ (nameNoSym "result") tyunknown_ (int_ 1)),
        (while_ (neqi_ (var_ "x") (int_ 1)) [
            (varassign_ (nameNoSym "result") (muli_ (var_ "result") (var_ "x"))), -- result = result*x
            (varassign_ (nameNoSym "x") (subi_ (var_ "x") (int_ 1)))  -- x = x-1
        ]),
        return_ (var_ "result")
    ]

    tyunknown_
    
    [
        param_ (nameNoSym "x") tyunknown_
    ]
in

let imperative_ast2 = funcdecl_ 
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

let selectionSort = funcdecl_
    [
        (nvardecl_ (nameNoSym "n") tyunknown_ (length_ (var_ "s"))),
        (nvardecl_ (nameNoSym "i") tyunknown_ (int_ 1)),
        (while_ (lti_ (var_ "i") (var_ "n")) [
            (nvardecl_ (nameNoSym "key") tyunknown_ (get_ (var_ "s") (var_ "i"))),
            (nvardecl_ (nameNoSym "j") tyunknown_ (subi_ (var_ "i") (int_ 1))),
            (while_ (and_ (geqi_ (var_ "j") (int_ 0)) (gti_ (get_ (var_ "s") (var_ "j")) (var_ "key"))) [
                (varassign_ (nameNoSym "s") (set_ (var_ "s") (addi_ (var_ "j") (int_ 1)) (get_ (var_ "s") (var_ "j")))),
                (varassign_ (nameNoSym "j") (subi_ (var_ "j") (int_ 1)))
            ]),
            (varassign_ (nameNoSym "s") (set_ (var_ "s") (addi_ (var_ "j") (int_ 1)) (var_ "key"))),
            (varassign_ (nameNoSym "i") (addi_ (var_ "i") (int_ 1)))
        ]),
        return_ (var_ "s")
    ]

    tyunknown_

    [
        param_ (nameNoSym "s") tyunknown_
    ]
in

-- useful functions
-- printLn (expr2str (translateFuncDecl imperative_ast));
-- dprintLn (translateFuncDecl imperative_ast);

--let program: String = strJoin "\n" [(expr2str (translateFuncDecl imperative_ast))]
let program: String = strJoin "\n" [
      "include \"mexpr/pprint.mc\"",
      "mexpr",
      "let selectionSort = ",
      expr2str (
        translateFuncDecl selectionSort
      ),
      --"in printLn (int2string (fib 6))"
      "in
      map (lam s. 
        print (int2string s);
        print \" \") (selectionSort [6, 1, 2, 5, 1, 3])"
      
    ] in
    printLn program;
    ()
    -- let tmpFilePath = sysTempFileMake () in
    -- writeFile tmpFilePath program;

    -- let ast = parseMCoreFile {{{{{{ defaultBootParserParseMCoreFileArg
    --   with keepUtests = false }
    --   with pruneExternalUtests = true }
    --   with externalsExclude = [] }
    --   with pruneExternalUtestsWarning = false }
    --   with eliminateDeadCode = true }
    --   with keywords = mexprExtendedKeywords } tmpFilePath
    -- in
    -- let ast = symbolize ast in
    -- let ast = typeCheck ast in
    -- -- printLn "here2"; we get up to here, lowering fails
    -- let ast = lowerAll ast in

    -- -- Compile the program
    -- let compileOCaml = lam libs. lam clibs. lam ocamlProg.
    -- let opt = {optimize = true, libraries = libs, cLibraries = clibs} in
    -- ocamlCompileWithConfig opt ocamlProg
    -- in
    -- let cunit: CompileResult = compileMCore ast (mkEmptyHooks compileOCaml) in

    -- --let res = cunit.run "" [] in
    -- let res = cunit.run "5" in
    -- utest res.stdout with 120 in
    -- utest res.stderr with "" in
    -- utest res.returncode with 0 in

    -- cunit.cleanup ();
    -- sysDeleteFile tmpFilePath;
    -- --tprintLn "";
    -- ()

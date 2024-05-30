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
    StmtMatch {target = target_expr, pattern = pattern_pat, thn = thn_stmtlist, els = else_stmtlist}

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
ImperativeMExpr + MCoreCompileLang + LowerNestedPatterns + MExprTypeCheck + BootParser
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

--let program: String = strJoin "\n" [(expr2str (translateFuncDecl imperative_ast))]
let program: String = strJoin "\n" [
      "include \"error.mc\"",
      "include \"map.mc\"",
      "include \"result.mc\"",
      "include \"seq.mc\"",
      "include \"string.mc\"",
      "include \"mexpr/info.mc\"",
      "include \"parser/lexer.mc\"",
      "mexpr",
      printLn (expr2str (translateFuncDecl imperative_ast));
    --   dprintLn (translateFuncDecl imperative_ast);
      expr2str (
        -- Wrap the generated expression in lambdas
        translateFuncDecl imperative_ast
      ),
      ""
    ] in

    let tmpFilePath = sysTempFileMake () in
    writeFile tmpFilePath program;

    let ast = parseMCoreFile {{{{{{ defaultBootParserParseMCoreFileArg
      with keepUtests = false }
      with pruneExternalUtests = true }
      with externalsExclude = [] }
      with pruneExternalUtestsWarning = false }
      with eliminateDeadCode = true }
      with keywords = mexprExtendedKeywords } tmpFilePath
    in
    let ast = symbolize ast in
    let ast = typeCheck ast in
    -- printLn "here2"; we get up to here, lowering fails
    let ast = lowerAll ast in

    -- Compile the program
    let compileOCaml = lam libs. lam clibs. lam ocamlProg.
    let opt = {optimize = true, libraries = libs, cLibraries = clibs} in
    ocamlCompileWithConfig opt ocamlProg
    in
    let cunit: CompileResult = compileMCore ast (mkEmptyHooks compileOCaml) in

    --let res = cunit.run "" [join ["\"", lrptInput parseTest, "\""]] in
    let res = cunit.run "5" in
    utest res.stdout with 120 in
    utest res.stderr with "" in
    utest res.returncode with 0 in

    cunit.cleanup ();
    sysDeleteFile tmpFilePath;
    --tprintLn "";
    ()

-- indirection layer (low-level DSL) for imperative language features (e.g. loops, mutable variables)
-- converts statement constructs to MExpr + a new TmFuncDecl expression construct
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
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

--TODO: delete MExprPrettyPrint
lang ImperativeMExpr = Ast + Sym + MExprPrettyPrint + MExprSym
    -- TODO: maybe change param.ident to param.name
    syn Expr =
        | TmFuncDecl {body: [Stmt], ty: Type, params: [{ty: Type, tyAnnot: Type, ident: Name}]}
    
    syn Stmt = 
        | StmtExpr {body: Expr}
        | StmtReturn {body: Expr}
        | StmtVarDecl {ident: Name, ty: Type, value: Expr}
        | StmtVarAssign {ident: Name, value: Expr}
        | StmtMatch {target : Expr,
                    pat : Pat,
                    thn : [Stmt],
                    els : [Stmt]}
        | StmtWhile {condition: Expr, body: [Stmt]}

    -- variables outside of the argument: immutable
    -- variables inside the argument: immutable
    -- rewritten variables: mutable
    -- every other case should be mutable

    -- env should contain a list of variables which are mutable (together with their symbols). (Could use a map here too but probably not necessary)
    sem fixReferences namelist = 
        | TmVar v -> 
            -- if we don't find it in nameList, then it's immutable
            let findResult = find (lam x. nameEqStr x v.ident) namelist in
            match findResult with Some x then
                deref_ (TmVar v)
            else
                TmVar v
            
        | x -> 
            smap_Expr_Expr (fixReferences namelist) x

    sem translateStmt names = 
        | StmtExpr e -> 
            -- what is the purpose of a standalone expression besides side effects?
            let contF = lam cont. 
                let x = nlet_ (nameNoSym "tmpexpr") tyunit_ e.body in 
                bind_ x cont
            in (names, contF)
        | StmtReturn r ->
            let contF = lam cont. r.body in
            (names, contF)
        | StmtVarDecl decl -> 
            let contF = lam cont. 
                let x = nlet_ decl.ident decl.ty (ref_ decl.value) in
                bind_ x cont
            in (cons (decl.ident) names, contF)
        | StmtVarAssign a -> 
            -- important to generate a symbol here to avoid naming the same thing twice
            let x = nulet_ (nameSym "tmpvassign") (modref_ (nvar_ a.ident) a.value) in
            let contF = lam cont. 
                bind_ x cont
            in (names, contF)
        | StmtMatch m ->
            match 
                mapAccumL translateStmt names m.thn
            with (newNames, thenTranslation) in
            match 
                mapAccumL translateStmt newNames m.els
            with (newNames1, elseTranslation) in
            let contF = lam cont.
                let then_body = foldr (lam continuationApp. lam acc. continuationApp acc) cont (thenTranslation) in
                let else_body = foldr (lam continuationApp. lam acc. continuationApp acc) cont (elseTranslation) in
                let match_expr = match_ m.target m.pat (then_body) (else_body) in
                ulet_ "tmpmatch" match_expr
            in (newNames1, contF)

        | StmtWhile w ->
            match 
                mapAccumL translateStmt names w.body
            with (newNames, bodyTranslation) in
            -- could use foldr here? 
            let translated_body = foldr (lam continuationApp. lam acc. continuationApp acc) unit_ (bodyTranslation) in
            let true_branch = bindall_ [translated_body, (appf1_ (var_ "tmp") unit_)] in
            let guard_with_recurse = match_ w.condition ptrue_ true_branch unit_ in

            -- recursive let tmp = lam ignore . guard_with_recurse in
            let contF = lam cont. 
                let x = (ureclet_ "tmp" (ulam_ "ignore" guard_with_recurse)) in
                let y = bind_ x (ulet_ "tmpapp" (appf1_ (var_ "tmp") unit_)) in 
                bind_ y cont
            in (newNames, contF)

    sem translateFuncDecl = 
        | TmFuncDecl func ->
            -- TODO: consider taking in the environment from outside this function
            let tyAnnot = func.ty in
            let env = symEnvDefault in
    
            -- want to create a TmLam for each variable in params. 
            -- with something like this for the bottommost lambda:
            -- tmLam NoInfo func.ty (nameSym "x") tyAnnot mexpr_body
            -- maybe just use an nlams_ here ?
            let params = reverse func.params in

            let wrapBodyParams = lam body_placeholder.
                match params with [] then
                    foldr
                        (lam param. lam acc. (tmLam (NoInfo ()) param.ty param.ident param.tyAnnot) acc) -- function that is being applied onto
                        (tmLam (NoInfo ()) tyunknown_ (nameNoSym "") tyunknown_ body_placeholder) -- bottom case; initial acc that is applied onto f
                        []
                else 
                    match params with [firstparam] ++ restparams in
                    foldr
                        (lam param. lam acc. (tmLam (NoInfo ()) param.ty param.ident param.tyAnnot) acc) -- function that is being applied onto
                        (tmLam (NoInfo ()) firstparam.ty firstparam.ident firstparam.tyAnnot body_placeholder) -- bottom case; initial acc that is applied onto f
                        restparams
            in
            let paramNames = map (lam x. x.ident) params in
            match 
                mapAccumL translateStmt paramNames func.body
            with (newNames, bodyTranslation) in
            let last_expr = ulet_ "tmp" unit_ in
            let mexpr_body = foldr 
                (lam continuationApp. lam acc. continuationApp acc) last_expr (bodyTranslation) in
            printLn (concat "env before final fix: " (foldr (lam x. lam acc. (concat (nameGetStr x) (concat " " acc))) "" newNames));
            let fixedTranslatedBody = fixReferences newNames mexpr_body in
            symbolizeExpr env (wrapBodyParams fixedTranslatedBody)
end

lang ImperativeMExprTestingPrerequisites = 
    ImperativeMExpr + MCoreCompileLang + LowerNestedPatterns + TypeCheck + BootParser
end
mexpr
use ImperativeMExprTestingPrerequisites in
let imperative_ast = funcdecl_ 
    [
        (nvardecl_ (nameNoSym "result") tyint_ (int_ 1)),
        (while_ (neqi_ (var_ "x") (int_ 1)) [
            (varassign_ (nameNoSym "result") (muli_ (var_ "result") (var_ "x"))), -- result = result*x
            (varassign_ (nameNoSym "x") (subi_ (var_ "x") (int_ 1)))  -- x = x-1
        ]),
        return_ (var_ "result")
    ]

    tyunknown_
    
    [
        param_ (nameNoSym "x") tyint_
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
      expr2str (bindall_ [
        -- Wrap the generated expression in lambdas
        translateFuncDecl imperative_ast
      ]),
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
    let ast = lowerAll ast in

    -- Compile the program
    let compileOCaml = lam libs. lam clibs. lam ocamlProg.
    let opt = {optimize = true, libraries = libs, cLibraries = clibs} in
    ocamlCompileWithConfig opt ocamlProg
    in
    let cunit: CompileResult = compileMCore ast (mkEmptyHooks compileOCaml) in

    --let res = cunit.run "" [join ["\"", lrptInput parseTest, "\""]] in
    let res = cunit.run 5 in
    utest res.stdout with 120 in
    utest res.stderr with "" in
    utest res.returncode with 0 in ()

    cunit.cleanup ();
    sysDeleteFile tmpFilePath;
    --tprintLn "";
    ()

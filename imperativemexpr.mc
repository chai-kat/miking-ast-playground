-- indirection layer (low-level DSL) for imperative language features (e.g. loops, mutable variables)
-- converts statement constructs to MExpr + a new TmFuncDecl expression construct
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/type.mc"
include "mexpr/symbolize.mc"
include "mexpr/pprint.mc"

--TODO: delete MExprPrettyPrint
lang ImperativeMExpr = Ast + Sym + MExprPrettyPrint
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


    -- sem symbolizeStmt (env : SymEnv) =
    --     | StmtExpr e -> symbolizeExpr e
    --     | StmtReturn r -> symbolizeExpr r
    --     | StmtVarDecl decl -> 
    --     | StmtVarAssign a -> 

    -- is there any way to do this without creating a match case for each pattern? 
    -- ideally we could just fix it in our stmts -> expr step, 
    -- but we still have to recurse into the expressions embedded in whichever expr we're translating
    -- want to use a smap


    -- we don't want to overwrite the function argument translation, 
    -- so this should only be called on translated_body? 

    -- variables outside of the argument: immutable
    -- variables inside the argument: immutable
    -- rewritten variables: mutable
    -- every other case should be mutable

    -- env should contain a list of variables which are mutable (together with their symbols). (Could use a map here too but probably not necessary)
    sem fixReferences namelist = 
        | TmVar v -> 
            -- if we don't find it in nameList, then it's immutable
            let findResult = find (lam x. eqi x v) namelist in
            match findResult with Some x then
                deref_ v
            else
                v 
            
        | x -> smap_Expr_Expr fixReferences x

    sem translateStmt names = 
        | StmtExpr e -> 
            -- what is the purpose of a standalone expression besides side effects?
            -- ulet_ "tmp" e.body
            let fixedExprBody = fixReferences names e.body in
            let contF = lam cont. 
                let x = nlet_ (nameNoSym "tmpexpr") tyunit_ fixedExprBody in 
                bind_ x cont
            in (names, contF)
        | StmtReturn r ->
            -- no need for the let?? could just return r.body directly no?
            -- (it's an expr, and nothing should come after it)
            -- nulet_ "tmp" r.body
            let fixedReturnBody = fixReferences names r.body in
            let contF = lam cont. fixedReturnBody in
            (names, contF)
        | StmtVarDecl decl -> 
            -- nlet_ decl.ident decl.ty (ref_ decl.value)
            let fixedDeclValue = fixReferences names decl.value in
            let contF = lam cont. 
                let x = nlet_ decl.ident decl.ty (ref_ fixedDeclValue) in
                bind_ x cont
            in (cons (decl.ident) names, contF)
        | StmtVarAssign a -> 
            -- important to generate a symbol here to avoid naming the same thing twice
            let x = nulet_ (nameSym "tmpvassign") (modref_ (nvar_ a.ident) a.value) in
            let contF = lam cont. 
                bind_ x cont
            in (names, contF)
        | StmtMatch m ->
            let contF = lam cont.
            match 
                foldr (
                    lam listVal. lam acc. 
                    match acc with (nameList, translatedExprs) in
                    match translateStmt nameList listVal with (newNameList, translatedExpr) in
                    (newNameList, cons translatedExpr translatedExprs) 
                    ) 
                    (names, [])
                    m.thn
                with (newNames, thenTranslation)
            in
            let then_body = foldr (lam continuationApp. lam acc. continuationApp acc) cont (reverse thenTranslation) in

            -- TODO: fix code duplication in foldr function
            match 
                foldr (
                    lam listVal. lam acc. 
                    match acc with (nameList, translatedExprs) in
                    match translateStmt nameList listVal with (newNameList, translatedExpr) in
                    (newNameList, cons translatedExpr translatedExprs) 
                    ) 
                    (names, [])
                    m.els
                with (newNames1, elseTranslation)
            in
            let else_body = foldr (lam continuationApp. lam acc. continuationApp acc) cont (reverse elseTranslation) in
            let match_expr = match_ m.target m.pat (then_body) (else_body) in
            (newNames1, ulet_ "tmpmatch" match_expr)

        | StmtWhile w ->
            match 
            foldr (
                lam listVal. lam acc. 
                match acc with (nameList, translatedExprs) in
                match translateStmt nameList listVal with (newNameList, translatedExpr) in
                (newNameList, cons translatedExpr translatedExprs) 
                ) 
                (names, [])
                w.body
            with (newNames, bodyTranslation)
            in
            -- could use foldl here? 
            let translated_body = foldr (lam continuationApp. lam acc. continuationApp acc) unit_ (reverse bodyTranslation) in
            let true_branch = bindall_ [translated_body, (appf1_ (var_ "tmptrue") unit_)] in
            let guard_with_recurse = match_ w.condition ptrue_ true_branch unit_ in

            -- recursive let tmp = lam ignore . guard_with_recurse in
            let contF = lam cont. 
                let x = (ureclet_ "tmp" (ulam_ "ignore" guard_with_recurse)) in
                let y = bind_ x (ulet_ "tmpapp" (appf1_ (var_ "tmp") unit_)) in 
                bind_ y cont
                -- match x with TmRecLets x in
                -- TmRecLets {x with inexpr = cont}
            (newNames, contF)

    -- let rec inner = lam .
    --     -- could change it so that it passes out a "newenv" for evaluation. 
    --     match condition with target
    --     then
    --         bindall_ (translateStmt body) -- works because every mutable var is a ref 
    --         inner
    --     else ()


    sem translateFuncDecl = 
        | TmFuncDecl func ->
            -- TODO: take in the environment from outside this function
            let env = symEnvDefault in
        
            -- TODO: consider passing params as references or renaming in body
            -- could rename all occurences inside the body if we do automatic reference conversion
            -- let mexpr_body = bindall_ 
            --         -- dprintLn (translateStmt x);
            --         -- printLn (expr2str (translateStmt x));
            --         -- printLn "\n";
            --         map translateStmt func.body
            -- in
            let last_expr = ulet_ "tmp" unit_ in
            let mexpr_body = foldr 
                (lam continuationApp. lam acc. continuationApp acc) last_expr (map translateStmt func.body) in

            -- make a let without a symbol here (let_ does nameNoSym internally)
            -- then symbolize later
            -- let_ 

            -- dprintLn mexpr_body;
            -- printLn (expr2str mexpr_body);


            -- let mexpr_body = symbolizeExpr env mexpr_body in
            let tyAnnot = func.ty in  -- TODO: fill out tyAnnot
    
            -- want to create a TmLam for each variable in params. 
            -- with something like this for the bottommost lambda:
            -- tmLam NoInfo func.ty (nameSym "x") tyAnnot mexpr_body
            
            -- TODO: reverse the parameters so we bind in the order the we're given
            -- let [firstparam | restparams] = reverse params in
            -- let params = func.params in
            let params = reverse func.params in
            let wrapBodyParams = lam body_placeholder.
                -- maybe just use an lams_ here ?
                match params with [] then
                    let translated_func = foldr
                        (lam param. lam acc. (tmLam (NoInfo ()) param.ty param.ident param.tyAnnot) acc) -- function that is being applied onto
                        (tmLam (NoInfo ()) tyunknown_ (nameNoSym "") tyunknown_ body_placeholder) -- bottom case; initial acc that is applied onto f
                        []
                    in
                    translated_func
                else
                    let firstparam = head params in
                    let restparams = tail params in
                    -- dprintLn (tmLam (NoInfo ()) firstparam.ty firstparam.ident firstparam.tyAnnot mexpr_body);
                    -- printLn (expr2str (tmLam (NoInfo ()) firstparam.ty firstparam.ident firstparam.tyAnnot mexpr_body));

                    let translated_func = foldr
                        (lam param. lam acc. (tmLam (NoInfo ()) param.ty param.ident param.tyAnnot) acc) -- function that is being applied onto
                        (tmLam (NoInfo ()) firstparam.ty firstparam.ident firstparam.tyAnnot body_placeholder) -- bottom case; initial acc that is applied onto f
                        restparams
                    in 
                    translated_func
                in
                let paramNames = map (lam x. x.ident) params in

                -- translated_func
                symbolizeExpr env (wrapBodyParams mexpr_body)
end

-- TmFuncDecl {
--     body = [
--         StmtReturn {
--             body = TmConst(0)
--         }
--     ],
--     ty = tyarrows_ [tyunit_, tyint_],
--     params = [],
--     ident = "main"
-- }

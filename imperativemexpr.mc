-- indirection layer (low-level DSL) for imperative language features (e.g. loops, mutable variables)
-- converts statement constructs to MExpr + a new TmFuncDecl expression construct
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/type.mc"
include "mexpr/symbolize.mc"
include "mexpr/pprint.mc"
include "mexpr/const-types.mc"

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
        -- handle modrefs differently
        -- TmApp {
        --     lhs = TmApp {
        --         lhs = TmConst {val = CModRef (), ...},
        --         rhs = refname (r),
        --         ...
        --     },
        --     rhs = value (v),
        --     ...
        -- }

        -- LHS = {LHS = (TmConst{val = (CModRef ())}), RHS = r}
        -- RHS = v

        | TmApp app1 -> 
            match app1.lhs with TmApp app2 then
                match app2.lhs with TmConst c then
                    match c.val with CModRef () then
                        TmApp {app1 with rhs = ((fixReferences namelist) app1.rhs)}
                        -- smap_Expr_Expr (fixReferences namelist) (TmApp app1)
                    else
                        -- in this case we know that app2 lhs is a constant but not a modref, so no need to care about it
                        -- let app2FixedRHS = TmApp {app2 with rhs = ((fixReferences namelist) app2.rhs)} in
                        -- TmApp {app1 with lhs = app2FixedRHS, rhs = (smap_Expr_Expr (fixReferences namelist) app1.rhs)}
                        -- means we can just reuse this case actually
                        smap_Expr_Expr (fixReferences namelist) (TmApp app1)
                        -- smap_Expr_Expr (fixReferences namelist) (TmApp app1)
                else
                    -- here lhs could be anything except a const, rhs could be anything
                    smap_Expr_Expr (fixReferences namelist) (TmApp app1)
            else 
                -- here app1.lhs is definitely not a TmApp, but that doesn't matter
                smap_Expr_Expr (fixReferences namelist) (TmApp app1)
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
            -- TODO: check that the type is not a reference already, then wrap with a reference
            let referencedType = decl.ty in
            -- let referencedType = mktyref_ decl.ty in

            let contF = lam cont. 
                let x = nlet_ decl.ident referencedType (ref_ decl.value) in
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

            --  do lam x. -> lam x. let x = ref x in ...
            let referencedParams = bindall_ (map (lam x. (nlet_ x.ident x.ty (ref_ (nvar_ x.ident)))) params) in
            let mexpr_body = bind_ referencedParams mexpr_body in

            -- printLn (concat "env before final fix: " (foldr (lam x. lam acc. (concat (nameGetStr x) (concat " " acc))) "" newNames));
            let fixedTranslatedBody = fixReferences newNames mexpr_body in
            symbolizeExpr env (wrapBodyParams fixedTranslatedBody)
end

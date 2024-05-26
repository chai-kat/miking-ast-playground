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

    -- env should contain a list of variables which are immutable (together with their symbols). 
    -- sem fixReferences env = 
    --     | TmVar v -> deref_ v
    --         -- if we don't find it in env, then it's a mutable variable
    --         match getSymbol env v with
            
    --     | x -> smap_Expr_Expr fixReferences x

    sem translateStmt = 
        | StmtExpr e -> 
            -- what is the purpose of a standalone expression besides side effects?
            -- ulet_ "tmp" e.body
            lam cont. 
                let x = nlet_ (nameNoSym "tmp") tyunit_ e.body in 
                -- type error because the cont is arrow type? is _a1 just a' or is it a -> a'?
                match x with TmLet x in
                TmLet {x with inexpr = cont}
        | StmtReturn r ->
            -- no need for the let?? could just return r.body directly no?
            -- (it's an expr, and nothing should come after it)
            -- nulet_ "tmp" r.body
            lam cont. r.body
        | StmtVarDecl decl -> 
            -- nlet_ decl.ident decl.ty (ref_ decl.value)
            lam cont. 
                let x = nlet_ decl.ident decl.ty (ref_ decl.value) in
                match x with TmLet x in
                TmLet {x with inexpr = cont}
        | StmtVarAssign a -> 
            lam cont. 
                let x = ulet_ "tmp" (modref_ (nvar_ a.ident) a.value) in
                match x with TmLet x in
                TmLet {x with inexpr = cont}
        | StmtMatch m ->
            lam cont. 
            let then_body = foldr (lam continuationApp. lam acc. continuationApp acc) cont (map translateStmt m.thn) in
            let else_body = foldr (lam continuationApp. lam acc. continuationApp acc) cont (map translateStmt m.els) in
            -- TODO: change this to avoid using bindall_, and instead fold the continuations
            -- let then_body = bindall_ (map translateStmt m.thn) in
            -- let else_body = bindall_ (map translateStmt m.els) in
            -- let then_body = bind_ then_body cont in
            -- let else_body = bind_ else_body cont in
            -- TODO: do we have to put the continuation at the end of both then_body and else_body?
            -- how would the types match otherwise
            let match_expr = match_ m.target m.pat (then_body) (else_body) in
            ulet_ "tmp" match_expr
            -- TmLet {ident = (nameNoSym "tmp"), tyAnnot = tyunit_, tyBody = tyunit_, body = match_expr, inexpr = cont, ty = tyunknown_, info = NoInfo ()}
        | StmtWhile w -> 
            let translated_body = foldr (lam continuationApp. lam acc. continuationApp acc) unit_ (map translateStmt w.body) in
            let true_branch = bindall_ [translated_body, (appf1_ (var_ "tmp") unit_)] in
            let guard_with_recurse = match_ w.condition ptrue_ true_branch unit_ in

            -- recursive let tmp = lam ignore . guard_with_recurse in
            lam cont. 
                let x = ureclet_ "tmp" (ulam_ "ignore" guard_with_recurse) in
                bind_ x cont
                -- match x with TmRecLets x in
                -- TmRecLets {x with inexpr = cont}

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

            -- maybe just use an lams_ here ?
            match params with [] then
                let translated_func = foldr
                    (lam param. lam acc. (tmLam (NoInfo ()) param.ty param.ident param.tyAnnot) acc) -- function that is being applied onto
                    (tmLam (NoInfo ()) tyunknown_ (nameNoSym "") tyunknown_ mexpr_body) -- bottom case; initial acc that is applied onto f
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
                    (tmLam (NoInfo ()) firstparam.ty firstparam.ident firstparam.tyAnnot mexpr_body) -- bottom case; initial acc that is applied onto f
                    restparams
                in 
                -- translated_func
            symbolizeExpr env translated_func
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

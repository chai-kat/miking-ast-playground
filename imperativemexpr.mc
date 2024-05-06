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

    sem translateStmt = 
        | StmtExpr e -> ulet_ "tmp" e.body -- what is the purpose of a standalone expression besides side effects?
        | StmtReturn r -> ulet_ "tmp" r.body --
        | StmtVarDecl decl -> ulet_ decl.ident decl.ty (ref_ decl.value) -- nlet_ decl.ident decl.ty (ref_ decl.value)
        | StmtVarAssign a -> modref_ (var_ a.ident) a.value 
        | StmtMatch m -> 
            let then_body = bindall_ (map translateStmt m.thn) in
            let else_body = bindall_ (map translateStmt m.els) in
            match_ m.target m.pat then_body else_body
        -- needs a match ,, isnt let bindings done when translating tmfuncdecl body? does reclet binding need to be done here?
        -- let rec loop = lam .
        --     match condition with true
        --     then
        --         translated_body // bindall_ (translateStmt body) 
        --         loop
        --     else ()
        

        -- recursive let tmp = lam ignore.
        --     match condition with true
        --     then
        --         let ignore_result = body in 
        --         tmp ()
        --     else
        --         ()

        | StmtWhile w -> 
            let translated_body = bindall_ (map translateStmt w.body) in
            let true_branch = bindall_ [translated_body, (appf1_ (var_ "tmp") unit_)] in
            let guard_with_recurse = match_ w.condition ptrue_ true_branch unit_ in

            -- recursive let tmp = lam ignore . guard_with_recurse in
            ureclet_ "tmp" (ulam_ "ignore" guard_with_recurse)

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
            let mexpr_body = bindall_ 
                    -- dprintLn (translateStmt x);
                    -- printLn (expr2str (translateStmt x));
                    -- printLn "\n";
                    map translateStmt func.body
            in
            
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

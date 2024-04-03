-- indirection layer (low-level DSL) for imperative language features (e.g. loops, mutable variables)
-- converts statement constructs to MExpr + a new TmFuncDecl expression construct
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"

lang ImperativeMExpr
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


    sem translateStmt = 
        | StmtExpr e -> e.body
        | StmtReturn r -> r.body
        | StmtVarDecl decl -> nlet_ decl.ident decl.ty (ref_ decl.value)
        | StmtVarAssign a -> modref_ a.ident a.value
        -- | 


    sem translateFuncDecl = 
    -- tmLam argument order:info, type, ident, tyAnnot, body
        | TmFuncDecl func -> 
            -- TODO: consider passing params as references or renaming in body
                -- could rename all occurences inside the body if we do automatic reference conversion
            let mexpr_body = bindall_ (map_ translateStmt func.body) in
            let tyAnnot = func.ty in  -- TODO: fill out tyAnnot

            -- want to create a TmLam for each variable in params. 
            -- with something like this for the bottommost lambda:
            -- tmLam NoInfo func.ty (nameSym "x") tyAnnot mexpr_body
            
            -- TODO: reverse the parameters so we bind in the order the we're given
            -- let [firstparam | restparams] = reverse params in
            
            let firstparam = head func.params in
            let restparams = tail func.params in
            foldr
                (lam param. lam acc. (tmLam (NoInfo ()) param.ty param.ident param.tyAnnot) acc) -- function that is being applied onto
                (tmLam (NoInfo ()) firstparam.ty firstparam.ident firstparam.tyAnnot mexpr_body) -- bottom case; initial acc that is applied onto f
                restparams
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

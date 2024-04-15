-- indirection layer (low-level DSL) for imperative language features (e.g. loops, mutable variables)
-- converts statement constructs to MExpr + a new TmFuncDecl expression construct
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/type.mc"
include "mexpr/symbolize.mc"
include "mexpr/pprint.mc"

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

    sem translateStmt = 
        | StmtExpr e -> ulet_ "tmp" e.body -- what is the purpose of a standalone expression besides side effects?
        | StmtReturn r -> ulet_ "tmp" r.body --
        | StmtVarDecl decl -> nlet_ decl.ident decl.ty (ref_ decl.value) -- nlet_ decl.ident decl.ty (ref_ decl.value)
        | StmtVarAssign a -> modref_ (nvar_ a.ident) a.value 
        -- | 


    sem translateFuncDecl = 
        | TmFuncDecl func ->
            let env = symEnvDefault in
        
            let mexpr_body = bindall_ 
                (map (lam x. translateStmt x) func.body) 
            in
    
            let tyAnnot = func.ty in  -- TODO: fill out tyAnnot
    
            let params = reverse func.params in

            match params with [] then -- when no param, need lam.
                let translated_func = foldr
                    (lam param. lam acc. (tmLam (NoInfo ()) param.ty param.ident param.tyAnnot) acc) -- function that is being applied onto
                    (tmLam (NoInfo ()) tyunknown_ (nameNoSym "") tyunknown_ mexpr_body) -- bottom case; initial acc that is applied onto f
                    []
                in
                -- translated_func
                symbolizeExpr env translated_func
            else
                let firstparam = head params in
                let restparams = tail params in

                let translated_func = foldr
                    (lam param. lam acc. (tmLam (NoInfo ()) param.ty param.ident param.tyAnnot) acc) -- function that is being applied onto
                    (tmLam (NoInfo ()) firstparam.ty firstparam.ident firstparam.tyAnnot mexpr_body) -- bottom case; initial acc that is applied onto f
                    restparams
                in 
                -- translated_func
                symbolizeExpr env translated_func
end

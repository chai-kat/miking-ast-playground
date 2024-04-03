-- indirection layer (low-level DSL) for imperative language features (e.g. loops, mutable variables)
-- converts statement constructs to MExpr + a new TmFuncDecl expression construct
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"

lang ImperativeMExpr
    syn Expr =
        | TmFuncDecl {body: [Stmt], ty: Type, params: [{ty: Type, tyAnnot: Type, ident: Name}] }
    
    syn Stmt = 
        | StmtMatch {target : Expr,
                    pat : Pat,
                    thn : [Stmt],
                    els : [Stmt]}
        | StmtReturn {body: Expr}
        | StmtWhile {condition: Expr, body: [Stmt]}
        | StmtVarDecl {ident: Name, ty: Type, value: Expr}
        | StmtVarAssign {ident: Name, value: Expr}
        | StmtExpr {body: Expr}

    sem translateStmt = 
        | StmtExpr -> let ??? = body in ...
        | StmtReturn -> body

    ulet_ 


    sem translateFuncDecl = 
    -- tmLam argument order:info, type, ident, tyAnnot
    -- acc could look something like:
    -- TmLet { inexpr = {fill this in with the fold (either a lambda or an expr)}  } -- similar to TreePpl
        | TmFuncDecl func -> 
            let mexpr_body = foldr (ulet_ translateStmt) acc func.body in
            let tyAnnot = ??? in  -- TODO: fill out tyAnnot
            tmLam NoInfo func.ty (nameSym "x") tyAnnot mexpr_body

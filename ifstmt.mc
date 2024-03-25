include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"

-- For variable assignments, we want to translate this: 
-- GFuncDef(TInt,"main",{},
--     SScope({
--         SVarDef(TInt,"x",EInt(1)) 
--         SVarAssign("x", EInt(2))
--         SReturn(EVar("x")
--   }))

--  into this:
-- TmLet {

-- For if statements, we want to translate this:
-- GFuncDef( TInt,"main",{},
--   SScope({
--     SIf(EBinOp(==,EVar("x"),EInt(0)),
--       SScope({SReturn(EInt(2))}))}))
--       SScope({SReturn(EInt(1))}),
-- 

int main() {
    if(x == 0) {
        return 2;
    }
    else {
        return 1;
    }
}
-- easier to have everything as a sequence of statements (ignoring scope construct to start with)
decl main: Unit -> Int = [
    printLn "hello";
    if eqi x 0 then [
        printLn "it's 2";   
        return 2
        ] 
    else [
        printLn "it's 1";
        return 1
        ]
    ]

-- potential AST:
StmtDecl {
    ident = "main",
    type = tyarrows_ [unit_, int_]
    body = [
        -- find some way to bind variable in all statements that follow
        StmtExpr {
            expr = TmApp("printLn", "hello")
        }
        StmtIf { --StmtMatch
            condition = TmApp(TmApp("eqi", TmVar("x'")), TmConst(0))
            ifTrueStmts = [
                StmtExpr {
                    expr = TmApp("printLn", "it's 2")
                }
                StmtReturn {
                    expr = TmConst(2)
                }
            ]
            ifFalseStmts = [
                StmtExpr {
                    expr = TmApp("printLn", "it's 1")
                }
                StmtReturn {
                    expr = TmConst(1)
                }
            ]
        }
    ]
}

decl main: Unit -> Int = [
    int x = 0
    while x == 0 then
        printLn "keep going";
        x = x + 1
        x = x + 2
    return 0
]

StmtFuncDecl {
    ident = "main",
    type = tyarrows_ [unit_, int_]
    body = [
        VarAssign("x", TmConst(0))
        StmtWhile {
            condition = (phi("x", "x2") == 0)
            body = [
                printLn,
                VarAssign("x1", Expr(x+1))
                VarAssign("x2", Expr(x1+2))
            ]
            parent = "main"
        }
        StmtReturn {
            expr = TmConst(0)
        }
    ] 
    parent = unit_
}

-- either handle direct code generation in imperative backend (e.g. to JVM)
-- or translate to existing functional fragments, by transforming the AST before code generation (e.g. to OCaml)


-- StmtFuncDecl
-- StmtMatch / StmtIfElse
-- StmtWhile

-- Into this:
-- | IfStmtTppl a ->
--   lam cont.
--   let contName = nameSym "ifCont" in
--   let contF = lam_ "" tyint_ cont in -- continuation function
--   let cont: Expr = withInfo a.info (app_ (nvar_ contName) (int_ 0)) in
--   TmLet {
--     ident = contName,
--     body = contF,
--     tyBody = tyunknown_,
--     tyAnnot = tyunknown_,
--     ty = tyunknown_,
--     info = a.info,
--     inexpr = TmMatch {
--       target = compileExprTppl context a.condition,
--       pat    = withInfoPat (get_ExprTppl_info a.condition) ptrue_,
--       thn    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifTrueStmts),
--       els    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifFalseStmts),
--       ty     = tyunknown_,
--       info   = a.info
--     }

lang IfStmtsCigrid
    syn FuncCigrid = 
        | GFuncDef {returnType: tyunknown_, label: String, args: [tyunknown_], body: StmtCigrid}

    syn StmtCigrid = 
        | SScope {stmts: [StmtCigrid]}
        | SVarDef {label: String, expr: ExprCigrid}
        | SVarAssign {label: String, expr: ExprCigrid}
        | SReturn {expr: ExprCigrid}
        | SIf {condition: ExprCigrid, ifTrueStmt: StmtCigrid, ifFalseStmt: StmtCigrid}

    syn BinOpCigrid = 
        | AddCigrid
        -- | SubCigrid
        | EqCigrid
        -- | NeqCigrid
        -- | LtCigrid
        -- | LteCigrid
        -- | GtCigrid
        -- | GteCigrid

    syn ExprCigrid =
        | EInt {value: Int}
        | EVar {label: String}
        | EBinOp {op: BinOpCigrid, left: ExprCigrid, right: ExprCigrid}

    -- TODO: convert this to presentation
    syn newStatements = 
        | StmtFuncDecl {ident: String, 
            -- maybe something like keepAliveVariables: [TmVar],
            params: [TmVar] | [TmConst],
            type: tyarrows_ [tyunknown_, tyunknown_], -- type can be inferred from params? 
            body: [Stmt], 
            parent: Stmt} 
        | StmtWhile {
            condition: ExprCigrid, 
            body: [Stmt], 
            parent: Stmt
        } -- make StmtFor? Or just rely on authors to do this ? 
        | StmtIfElse {
            condition: ExprCigrid,
            ifTrueStmts: [Stmt], 
            ifFalseStmts: [Stmt], 
            parent: Stmt
        }
        | 



    -- sem compileFuncCigrid: FuncCigrid -> Expr
    sem compileFuncCigrid = 
        | FuncCigrid f -> 
            TmLam {
                ident = f.label,
                body = compileStmtCigrid f.body,
                ty = tyunknown_,
                tyAnnot = tyunknown_,
                tyParam = tyunknown_,
                info = NoInfo () -- found this in src/boot/lib/repl.ml @ line 144
            }
        
    
    -- sem compileStmtCigrid: StmtCigrid -> Expr
    sem compileStmtCigrid =
        | SScope a -> map (compileStmtCigrid) a.stmts
        | SVarDef a -> 
            TmLet {
                ident = a.label,
                body = compileExprCigrid a.expr,
                tyBody = tyunknown_,
                tyAnnot = tyunknown_,
                -- info = a.info,
                info = NoInfo () -- found this in src/boot/lib/repl.ml @ line 144
            }
        | SVarAssign a ->
            TmLet {
                ident = a.label,
                body = compileExprCigrid a.expr,
                tyBody = tyunknown_,
                tyAnnot = tyunknown_,
                -- info = a.info,
                info = NoInfo () -- found this in src/boot/lib/repl.ml @ line 144
            }
        | SReturn a -> compileExprCigrid a.expr
        | SIf a -> 
            -- let contName = nameSym "ifCont" in
            -- let contF = lam_ "" tyint_ cont in -- continuation function
            -- let cont: Expr = withInfo a.info (app_ (nvar_ contName) (int_ 0)) in
            -- TmLet {
            --     ident = contName,
            --     body = contF,
            --     tyBody = tyunknown_,
            --     tyAnnot = tyunknown_,
            --     ty = tyunknown_,
            --     info = a.info,
            --     inexpr = TmMatch {
            --         target = compileExprCigrid a.condition,
            --         pat    = withInfoPat (get_ExprCigrid_info a.condition) ptrue_,
            --         thn    = foldr (lam f. lam e. f e) cont (map (compileStmtCigrid) a.ifTrueStmts),
            --         els    = foldr (lam f. lam e. f e) cont (map (compileStmtCigrid) a.ifFalseStmts),
            --         ty     = tyunknown_,
            --         info   = a.info
            --     }
            -- }
        -- let ifContinuation = continuationFunction in 
        -- in mexpr style should always be this: (no guards)
        --     match a.condition with pat
        --     then thn
        --     else els

    sem compileBinopCigrid =
        | AddCigrid -> CAddi
        | SubCigrid -> CSubi
        | EqCigrid -> CEqi
        | NeqCigrid -> CNeqi
        | LtCigrid -> CLti
        | LteCigrid -> CLeqi
        | GtCigrid -> CGti
        | GteCigrid -> CGeqi

    sem compileExprCigrid =
        | EInt a ->
            TmConst {
                val : a.value,
                ty: tyint_,
                info: NoInfo ()
            }
        | EVar a ->
            TmVar{
                ident : a.label,
                ty: tyunknown_,
                info: NoInfo (),
                frozen: Bool
            }
        | EBinOp a ->
            (compileBinopCigrid a.op, [compileExprCigrid a.left, compileExprCigrid a.right]) -- this is how we do it in the mexpr evaluator (mexpr/eval.mc)
        

    -- Cigrid: x = 5+5; VarAssign("x", BinOp(Add, Int(5), Int(5))
    -- Miking: let x = 5 + 5; TmLet(ident = x, body = , tyBody = tyunknown_, tyAnnot = tyunknown_, info = NoInfo)

    -- This is how stuff is represented in mexpr/eval.mc (used above)
    -- sem delta info =
    -- | (CAddi _, [TmConst {val = CInt n1}, TmConst (t & {val = CInt n2})]) ->
    --   TmConst {t with val = CInt {val = addi n1.val n2.val}}


    -- | AssignStmtTppl a ->
    --     lam cont. TmLet {
    --       ident = a.var.v,
    --       tyBody = tyunknown_,
    --       tyAnnot = optionMapOr tyunknown_ compileTypeTppl a.ty,
    --       body =  compileExprTppl context a.val,
    --       inexpr = cont,
    --       ty = tyunknown_,
    --       info = a.info
    --     }
end
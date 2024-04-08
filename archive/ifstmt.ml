type bop = LessEqual | GreaterEqual | Equal | NotEqual

type ty = 
| Tyunknown_
| Tyint_

type expr = 
  | EVar of string
  | EInt of int
  | EChar of string
  | EString of string

type stmt =
  | SExpr of expr
  | SVarAssign of string * expr (* x=5 "string: x, expr: 5" *)
  | SScope of stmt list
  | SIf of expr * stmt * stmt option
  | SWhile of expr * stmt

type match_ast = 
  | TmMatch of {target: expr; pat: expr; thn: stmt; els: stmt}
  | TmLet of {ident: string; body: expr; tyBody: ty; tyAnnot: ty; ty: ty; inexpr: expr}
  | EBinOp of bop * expr * expr

(* -- TODO for Daniel: have C compiler handle f()
| IfStmtTppl a ->
  lam cont.
  let contName = nameSym "ifCont" in
  let contF = lam_ "" tyint_ cont in -- continuation function
  let cont: Expr = withInfo a.info (app_ (nvar_ contName) (int_ 0)) in
  TmLet {
    ident = contName,
    body = contF,
    tyBody = tyunknown_,
    tyAnnot = tyunknown_,
    ty = tyunknown_,
    info = a.info,
    inexpr = TmMatch {
      target = compileExprTppl context a.condition,
      pat    = withInfoPat (get_ExprTppl_info a.condition) ptrue_,
      thn    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifTrueStmts),
      els    = foldr (lam f. lam e. f e) cont (map (compileStmtTppl context) a.ifFalseStmts),
      ty     = tyunknown_,
      info   = a.info
    }
  } *)


(* possible ast 
  
  

*)

let ident = body in 
  match target with
  | pat -> thn
  | _ -> els

match x with
| 1
| 2
| 3 


GFuncDef( TInt,"main",{},
  SScope({
    SIf(EBinOp(==,EVar("x"),EInt(0)),
      SScope({SReturn(EInt(1))}),
      SScope({SReturn(EInt(2))}))}))


EBinOp() -> match target with 
SIf(EBinOp(_) as target, SScope(_) as x, SScope(_) as y)
SScope() -> thn
SScope() -> els


let translateMatchToIf 


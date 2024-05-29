include "mexpr/ast.mc"
include "./imperativemexpr.mc"
include "mexpr/type.mc"

let stmtexpr_ = use ImperativeMExpr in
    lam e. StmtExpr {body = e}

let return_ = use ImperativeMExpr in
    lam e. StmtReturn {body = e}

-- named
-- ident: Name, value: Expr
let nvardecl_ = use ImperativeMExpr in
    lam ident. lam ty. lam value. StmtVarDecl {ident = ident, ty = ty, value = value}

-- named untyped
let nuvardecl_ = use ImperativeMExpr in
    lam ident. lam value. StmtVarDecl {ident = ident, ty = tyunknown_, value = value}

let varassign_ = use ImperativeMExpr in
    lam ident. lam e. StmtVarAssign {ident = ident, value = e}

let stmtmatch_ = use ImperativeMExpr in
    lam target_expr. lam pattern_pat. lam thn_stmtlist. lam else_stmtlist.
    StmtMatch {target = target_expr, pattern = pattern_pat, thn = thn_stmtlist, els = else_stmtlist}

let while_ = use ImperativeMExpr in
    lam condexpr. lam stmtlist. StmtWhile {condition = condexpr, body = stmtlist}

let param_ = lam ident. lam ty. {ty = ty, tyAnnot = ty, ident = ident}

let funcdecl_ = use ImperativeMExpr in
    lam b. lam ty. lam params. TmFuncDecl {body = b, ty = ty, params = params}

let ufuncdecl_ = use ImperativeMExpr in
    lam b. lam params. TmFuncDecl {body = b, ty = tyunknown_, params = params}

let unitfuncdecl_ = use ImperativeMExpr in
    lam b. TmFuncDecl {body = b, ty = tyunit_, params = []}



type uop =
  | Factorial | Tilde | Umin

type bop =
  | Add | Sub | Mul | Div | Mod
  | Less | Greater | LessEqual | GreaterEqual | Equal | NotEqual
  | BitwiseAnd | BitwiseOr | LogicalAnd | LogicalOr
  | ShiftL | ShiftR

type type_specifier =
  | TVoid
  | TInt 
  | TChar
  | TIdent of string
  | TPoint of type_specifier

type expr =
  | EVar of string
  | EInt of int
  | EChar of string
  | EString of string
  | EBinOp of bop * expr * expr
  | EUnOp of uop * expr
  | ECall of string * expr list
  | ENew of type_specifier * expr
  | EArrayAccess of string * expr * string option

type stmt =
  | SExpr of expr
  | SVarDef of type_specifier * string * expr
  | SVarAssign of string * expr
  | SArrayAssign of string * expr * string option * expr
  | SScope of stmt list
  | SIf of expr * stmt * stmt option
  | SWhile of expr * stmt
  | SBreak 
  | SReturn of expr option 
  | SDelete of string

type global =
  | GFuncDef of type_specifier * string * (type_specifier * string) list * stmt
  | GFuncDecl of type_specifier * string * (type_specifier * string) list
  | GVarDef of type_specifier * string * expr
  | GVarDecl of type_specifier * string
  | GStruct of string * (type_specifier * string) list

type program =
  | Prog of global list




(* int x = 5 
   x = 6
   printf x
*)

(* 
  int x = 6
  if (x == 6) {
    int x = 5
    printf(x)
  }
  else {
    printf(x)
  }
*)

let x = 5 in 
let x = 6 in
print_endline x;;

int main() {
      if (x == 0) {
        return 1; 
      }
      else {
        return 2;
      }
}

GFuncDef(TInt,"main",{},
    SScope({
        SIf(EBinOp(==,EVar("x"),EInt(0)),
            SScope({SReturn(EInt(1))}),
        SScope({SReturn(EInt(2))}))}))

let main = if x = 0 then 1 else 2
match ifstmt with
| truecase -> true
| falsecase -> false
        
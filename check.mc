include "mexpr/pprint.mc"
mexpr

let factorial = lam x.
  let x1 = ref x in
  let result = ref 1 in
  recursive let tmpWhile = lam ignore.
    match neqi (deref x1) 1 with true then
      let tmpvassign = modref result (muli (deref result) (deref x1)) in
      let tmpvassign1 = modref x1 (subi (deref x1) 1) in
      tmpWhile {}
    else
      None {}
  in
  match tmpWhile {} with Some x2 then 
    deref x2
  else
    deref result
in 

printLn (int2string (factorial 5))

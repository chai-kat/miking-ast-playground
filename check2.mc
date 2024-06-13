include "mexpr/pprint.mc"
mexpr
let fibonacci = lam x.
  let x1 = ref x in
  let a = ref 0 in
  let b = ref 1 in
  let sum = ref 0 in
  let n = ref 1 in
  match lti (deref x1) 2 with true then
    deref x1
  else
    recursive let tmpWhile = lam ignore. 
      match neqi (deref n) (deref x1) with true then
        let tmpvassign = modref sum (addi (deref a) (deref b)) in
        let tmpvassign1 = modref a (deref b) in
        let tmpvassign2 = modref b (deref sum) in
        let tmpvassign3 = modref n (addi (deref n) 1) in
        tmpWhile {}
      else
        None {}
    in
    match tmpWhile {} with Some x2 then
      deref x2
    else
      deref sum
in

printLn (int2string (fibonacci 5))

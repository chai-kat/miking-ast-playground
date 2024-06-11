include "mexpr/pprint.mc"
mexpr
let fib = lam x.
  let x1 = ref x in
  let a = ref 0 in
  let b = ref 1 in
  let sum = ref 0 in
  let n = ref 1 in
  match lti (deref x1) 2 with true 
    then
        deref x1
    else
        recursive let tmp = lam ignore.
          match neqi (deref n) (deref x1) with true
            then
                let tmpvassign = modref sum (addi (deref a) (deref b)) in
                let tmpvassign1 = modref a (deref b) in
                let tmpvassign2 = modref b (deref sum) in
                let tmpvassign3 = modref n (addi (deref n) 1) in
                tmp ()
            else
                ()
        in
    let tmpapp = tmp () in
    deref sum
in 
printLn (int2string (fib 6))

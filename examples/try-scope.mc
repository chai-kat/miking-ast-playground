include "mexpr/pprint.mc"

mexpr
let k = ref 5 in
let f = lam x. 
    addi x (deref k) 
in 
printLn (int2string (f (deref k)))
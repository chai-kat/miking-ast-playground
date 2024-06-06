include "mexpr/pprint.mc"

mexpr
let x = ref 5 in
let y = ref x in
printLn (int2string (deref (deref y)))
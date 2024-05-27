include "mexpr/pprint.mc"

mexpr 
let fact: Int -> Int = lam x: Int.
  let x1 = ref x in
  printLn (int2string (deref x1));
  let result = ref 1 in
  printLn (int2string (deref result));
  recursive let tmp = lam ignore.
        printLn (int2string (deref x1));
        match neqi (deref x1) 1 with true
        then
          let tmp1 = modref result (muli (deref result) (deref x1)) in
          let tmp2 = modref x1 (subi (deref x1) 1) in
          tmp ()
        else
          ()
  in
  tmp ();
  (deref result)
in
printLn (int2string (fact 5))
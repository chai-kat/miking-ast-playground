include "mexpr/pprint.mc"

mexpr
let fib = lam x: Int.
    let x1 = ref 0 in
    let x2 = ref 1 in
    let sum = ref 0 in
    let n = ref 1 in

    recursive let tmp = lam ignore.
        match neqi (deref n) x with true then
            modref sum (addi (deref x1) (deref x2));
            modref x1 (deref x2);
            modref x2 (deref sum);
            modref n (addi (deref n) 1);
            tmp ()
        else
            ()
    in

    let ans = 
        if lti x 2 then x 
        else 
            tmp ();
            deref sum
    in
    
    ans
in
-- printLn (int2string (fib 0));
-- printLn (int2string (fib 1));
-- printLn (int2string (fib 2));
-- printLn (int2string (fib 3));
-- printLn (int2string (fib 4));
-- printLn (int2string (fib 5));
-- printLn (int2string (fib 6))

print (int2string (fib 1000000000))
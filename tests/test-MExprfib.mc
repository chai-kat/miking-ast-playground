include "mexpr/pprint.mc"

mexpr
recursive let fib = lam a. lam b. lam n.
    match n with 0 then
      a
    else
      fib b (addi a b) (subi n 1)
in
print(int2string((fib 0 1 1000000000)))


-- let fib = lam x: Int.
--     let x1 = ref 0 in
--     let x2 = ref 1 in
--     let sum = ref 0 in
--     let n = ref 1 in

--     recursive let tmp = lam ignore.
--         match neqi (deref n) x with true then
--             modref sum (addi (deref x1) (deref x2));
--             modref x1 (deref x2);
--             modref x2 (deref sum);
--             modref n (addi (deref n) 1);
--             tmp ()
--         else
--             ()
--     in

--     let ans = 
--         if lti x 2 then x 
--         else 
--             tmp ();
--             deref sum
--     in
    
--     ans
-- in

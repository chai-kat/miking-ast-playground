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

utest (fib 6) with 8 in
()


mexpr 
use Foo in
let imperative_ast = funcdecl_ 
    [
        (nvardecl_ (nameNoSym "x1") tyint_ (int_ 0)),
        (nvardecl_ (nameNoSym "x2") tyint_ (int_ 1)),
        (nvardecl_ (nameNoSym "x1") tyint_ (int_ 0)),
        (nvardecl_ (nameNoSym "sum") tyint_ (int_ 0)),
        (nvardecl_ (nameNoSym "n") tyint_ (int_ 1)),
        (while_ (neqi_ (var_ "n") (var_ x))) [
            (varassign_ (nameNoSym "sum") (addi_ (var_ "x1") (var_ "x2"))),
            (varassign_ (nameNoSym "x1") (var_ "x2")),
            (varassign_ (nameNoSym "x2") (var_ "sum")),
            (varassign_ (nameNoSym "n") (addi_ (var_ "n") (int_ 1)))
        ]
        return_ (var_ "sum")


        (while_ (neqi_ (var_ "x") (int_ 1)) [
            (varassign_ (nameNoSym "result") (muli_ (var_ "result") (var_ "x"))), -- result = result*x
            (varassign_ (nameNoSym "x") (subi_ (var_ "x") (int_ 1)))  -- x = x-1
        ]),
        return_ (var_ "result")
    ]

    tyunknown_
    
    [
        param_ (nameNoSym "x") tyint_
    ]
 in

 fibonacci(x):
    a = 0;
    b = 1;
    sum = 0;
    n = 1;
    if (x < 2) {
        return x;
    }
    while (n != x) {
        sum = a + b;
        a = b;
        b = sum;
        n++;
    }
    return sum;
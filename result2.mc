include "mexpr/pprint.mc"
mexpr
let fib = 
lam x.
  let x1 =
    ref
      x
  in
  let x11 =
    ref
      0
  in
  let x2 =
    ref
      1
  in
  let sum =
    ref
      0
  in
  let n =
    ref
      1
  in
  let tmpmatch =
    match
      lti
        (deref
           x1)
        2
    with
      true
    then
      deref
        x1
    else
      recursive
        let tmp =
          lam ignore.
            match
              neqi
                (deref
                   n)
                (deref
                   x1)
            with
              true
            then
              let tmpvassign =
                modref
                  sum
                  (addi
                     (deref
                        x11)
                     (deref
                        x2))
              in
              let tmpvassign1 =
                modref
                  x11
                  (deref
                     x2)
              in
              let tmpvassign2 =
                modref
                  x2
                  (deref
                     sum)
              in
              let tmpvassign3 =
                modref
                  n
                  (addi
                     (deref
                        n)
                     1)
              in
              tmp
                {}
            else
              {}
      in
      let tmpapp =
        tmp
          {}
      in
      deref
        sum
  in
  deref
    sum
in printLn (int2string (fib 5))

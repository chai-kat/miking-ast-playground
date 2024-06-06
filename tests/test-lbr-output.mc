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
            5
            -- else
            --   tmp {}
          else
            {}
    in
    let tmpapp =
      tmp
        {}
    in
    match
      tmpapp
    with
      {}
    then
      deref
        sum
    else
      tmpapp
in printLn (int2string (fib 6))

include "mexpr/pprint.mc"

mexpr
let fact = lam x.
  let x1 =
    ref
      x
  in
  let result =
    ref
      1
  in
  recursive
    let tmp =
      lam ignore.
        match
          neqi
            (deref
               x1)
            1
        with
          true
        then
          let tmpvassign =
            modref
              result
              (muli
                 (deref
                    result)
                 (deref
                    x1))
          in
          let tmpvassign1 =
            modref
              x1
              (subi
                 (deref
                    x1)
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
    result
in 
printLn (int2string (fact 5))
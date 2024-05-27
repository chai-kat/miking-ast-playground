include "mexpr/pprint.mc"

mexpr 
let fact = lam x: Int.
  let result: Int =
    ref
      1
  in
  recursive
    let tmp =
      lam ignore.
        match
          x
        with
          true
        then
          let tmp =
            modref
              result
              (muli
                 result
                 x)
          in
          let tmp =
            modref
              x
              (subi
                 x
                 1)
          in
          tmp
            {}
        else
          {}
  in
  result

in
dprintLn (int2string (fact 5))
include "mexpr/pprint.mc"
mexpr
let fact = lam n: Int.
  let n1: Int =
    ref
      n
  in
  match
    eqi
      (deref
         n1)
      0
  with
    true
  then
    1
  else
    let result: Int =
      ref
        1
    in
    recursive
      let tmpWhile =
        lam ignore.
          match
            neqi
              (deref
                 n1)
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
                      n1))
            in
            let tmpvassign1 =
              modref
                n1
                (subi
                   (deref
                      n1)
                   1)
            in
            tmpWhile
              {}
          else
            None
              {}
    in
    match
      tmpWhile
        {}
    with
      Some x
    then
      x
    else
      deref
        result
in
printLn (int2string (fact 5))
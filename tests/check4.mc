include "mexpr/pprint.mc"
mexpr
let compare = 
lam a.
  lam b.
    let b1 =
      ref
        b
    in
    let a1 =
      ref
        a
    in
    match
      gti
        (deref
           a1)
        (deref
           b1)
    with
      true
    then
      deref
        a1
    else
      deref
        b1
in
let max = 
lam arr.
  let arr1 =
    ref
      arr
  in
  let result =
    ref
      0
  in
  let i =
    ref
      0
  in
  recursive
    let tmp =
      lam ignore.
        match
          lti
            (deref
               i)
            (length
               (deref
                  arr1))
        with
          true
        then
          let tmpvassign =
            modref
              result
              (compare
                 (deref
                    result)
                 (get
                    (deref
                       arr1)
                    (deref
                       i)))
          in
          let tmpvassign1 =
            modref
              i
              (addi
                 (deref
                    i)
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
in printLn (int2string (max [1, 2, 4, 3, 1]))
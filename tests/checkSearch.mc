include "mexpr/pprint.mc"
mexpr
let linearSearch = 
lam arr.
  lam target.
    let target1 =
      ref
        target
    in
    let arr1 =
      ref
        arr
    in
    let index =
      ref
        0
    in
    recursive
      let tmpWhile =
        lam ignore.
          match
            lti
              (deref
                 index)
              (length
                 (deref
                    arr1))
          with
            true
          then
            match
              eqi
                (get
                   (deref
                      arr1)
                   (deref
                      index))
                (deref
                   target1)
            with
              true
            then
              Some
                (deref
                   index)
            else
              let tmpvassign =
                modref
                  index
                  (addi
                     (deref
                        index)
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
      (negi 1)
in printLn (int2string (linearSearch [1, 3, 1, 4, 3] 4))

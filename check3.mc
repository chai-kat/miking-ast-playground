include "mexpr/pprint.mc"
mexpr
let selectionSort = 
lam s.
  let s1 =
    ref
      s
  in
  let n =
    ref
      (length
         (deref
            s1))
  in
  let i =
    ref
      1
  in
  recursive
    let tmp =
      lam ignore.
        match
          lti
            (deref
               i)
            (deref
               n)
        with
          true
        then
          let key =
            ref
              (get
                 (deref
                    s1)
                 (deref
                    i))
          in
          let j =
            ref
              (subi
                 (deref
                    i)
                 1)
          in
          recursive
            let tmp1 =
              lam ignore1.
                match
                  match
                    geqi
                      (deref
                         j)
                      0
                  with
                    true
                  then
                    gti
                      (get
                         (deref
                            s1)
                         (deref
                            j))
                      (deref
                         key)
                  else
                    false
                with
                  true
                then
                  let tmpvassign2 =
                    modref
                      s1
                      (set
                         (deref
                            s1)
                         (addi
                            (deref
                               j)
                            1)
                         (get
                            (deref
                               s1)
                            (deref
                               j)))
                  in
                  let tmpvassign3 =
                    modref
                      j
                      (subi
                         (deref
                            j)
                         1)
                  in
                  tmp1
                    {}
                else
                  {}
          in
          let tmpapp1 =
            tmp1
              {}
          in
          let tmpvassign =
            modref
              s1
              (set
                 (deref
                    s1)
                 (addi
                    (deref
                       j)
                    1)
                 (deref
                    key))
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
    s1
in
      map (lam s. 
        print (int2string s);
        print " ") (selectionSort [6, 1, 2, 5, 1, 3])

include "mexpr/pprint.mc"
mexpr
let insertionSort = 
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
    let tmpWhile =
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
            let tmpWhile1 =
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
                  tmpWhile1
                    {}
                else
                  None
                    {}
          in
          match
            tmpWhile1
              {}
          with
            Some x1
          then
            Some
              x1
          else
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
      s1
in
      map (lam s. 
        print (int2string s);
        print " ") (insertionSort [6, 1, 2, 5, 1, 3])

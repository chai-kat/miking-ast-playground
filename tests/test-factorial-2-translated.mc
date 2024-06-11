let fact = lam n: Int.
  let n1: Int = ref n in
  match eqi (deref n1) 0 with true then 
    1
  else
    let result: Int = ref 1 in
    recursive let tmp = lam ignore.
      match (neqi (deref n1) 1) with true then
        let tmpvassign = modref result
            (muli (deref result) (deref n1))
        in
        let tmpvassign1 = modref n1
            (subi (deref n1) 1)
        in
        tmp {}
      else {}
    in
    let tmpapp = tmp {} in
    deref result

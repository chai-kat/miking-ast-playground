while a <= 1
  a++
  if a == 10
    return 5
  end

end

-- need to translate | StmtWhile {condition: Expr, body: [Stmt]} // StmtWhile should have a Pat no?

let outer = lam .
--    let k = scope
    let rec inner = lam .
        bindall_ (translateStmt body) -- works because every mutable var is a ref
        -- could change it so that it passes out a "newenv" for evaluation. 
        match condition with target
        then inner
        else ()
    let x = match condition with target
    then inner
    else ()

-- could change it so that it passes out a "newenv" for evaluation. 

-- works because every mutable var is a ref, including the loop exit condition var

let rec loop = lam .
  match condition with target
  then
      bindall_ (translateStmt body) 
      inner
  else ()



-- int add1(int x) {
--     return x + 1;
-- }

let add1 = lam x.
  let x1 = ref x in
  x1+1
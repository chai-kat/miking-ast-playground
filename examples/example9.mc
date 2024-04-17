include "mexpr/pprint.mc"
lang Foo = MExprPrettyPrint end

mexpr
    use Foo in
    let main = lam x. 
        dprintLn (deref x)
    in let k = ref "end"
    in main k

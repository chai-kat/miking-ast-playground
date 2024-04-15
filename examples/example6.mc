include "mexpr/pprint.mc"
lang Foo = MExprPrettyPrint end

mexpr
    use Foo in
    let main = lam x. 
        match x with 5
            then "1"
            else "2"
        let y = "5" in ()
    in 
        dprintLn (main 5)

mexpr-extended
    use Foo in
    let main = lam x. 
        match x with 5
            then return x+5
            else x=5 --what if no return here
        y = x + 2 in ()
    in 
        dprintLn (main 5)

    lam x. 
        let cont = lam env.
            match env.x with 5
            then return env.x + 5
            else x = 5 
        let main1 = cont env in
            match main1 with <value>
            then value
            else main1 env -- if we get a lambda here (represent with a tuple (value/lambda, expr type return thingy here))

        let y = ref 2 -- illegal in MExpr?

-- top-level lambda which calls the first continuation. 
-- every time we hit a branch we add a continuation to the stack?¿ 




int main() {
    return 0;
    print 5 ; // return statement can't be reached
}

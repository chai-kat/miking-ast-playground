include "mexpr/pprint.mc"

mexpr

recursive let fact = lam n. lam acc.
    match n with 1 then
      acc
    else
      fact (subi n 1) (muli acc n)
in 

print(int2string(fact 1000000000 1))
include "mexpr/pprint.mc"

mexpr
let max = lam array.

    let compare = lam a. lam b.
        match (gti a b) with true then
            a
        else
            b
    in

    recursive let fold = lam acc. lam seq.
        match seq with [] then 
            acc
        else
            fold (compare acc (get seq 0)) (subsequence seq 1 (length array))
    in

    fold (get array 0) (subsequence array 1 (length array))

in


 printLn (int2string (max [1 4 5 2]))
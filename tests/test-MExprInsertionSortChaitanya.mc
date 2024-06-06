include "mexpr/pprint.mc"
mexpr
let calls = ref 0 in
let insertionSort = lam array.
    match (lti (length s) 2) with true then array
    else
        recursive let insert_sorted = lam element. lam sorted_arr.
            printLn (int2string (deref calls));
            match sorted_arr with [] then
                [element]
            else
                match (leqi  element (get sorted_arr 0)) with true then
                    cons element sorted_arr
                else
                    cons (get sorted_arr 0) (insert_sorted element (subsequence sorted_arr 1 (length sorted_arr)))
        in

        recursive let sort_recursively = lam unsorted_arr. lam sorted_arr.
            modref calls (addi (deref calls) 1);
            print "insert_sorted calls: ";
            match unsorted_arr with [] then
                sorted_arr
            else
                sort_recursively (subsequence unsorted_arr 1 (length unsorted_arr)) (insert_sorted (get unsorted_arr 0) sorted_arr)
        in

        sort_recursively (subsequence array 1 (length array)) [(get array 0)]
in

recursive let fill_sequence = lam s. lam pos. lam len. 
    match (eqi len 0) with true then s
    else
        (fill_sequence (cons pos s) (addi pos 1)) (subi len 1)
in let sequence = fill_sequence [] 0 100
in
-- (sequence)
(insertionSort sequence)



def insertion_sort(arr):
    if len(arr) <= 1:
        return arr

    def insert_sorted(element, sorted_arr):
        if not sorted_arr:
            return [element]
        elif element <= sorted_arr[0]:
            return [element] + sorted_arr
        else:
            return [sorted_arr[0]] + insert_sorted(element, sorted_arr[1:])

    def sort_recursively(unsorted_arr, sorted_arr):
        if not unsorted_arr:
            return sorted_arr
        else:
            return sort_recursively(unsorted_arr[1:], insert_sorted(unsorted_arr[0], sorted_arr))

    return sort_recursively(arr[1:], [arr[0]])


include "mexpr/pprint.mc"

mexpr
let insertionSort = lam array.
    match (lti (length s) 2) with true then array

    let recursive let insert_sorted = lam element. lam. sorted_arr.
        match sorted_arr with []

    let s = ref s
    let n = ref (length s) in
    let i = ref 1 in
    recursive let tmp1 = lam ignore.
        match lti (deref i) (deref n) with true then
            let key = ref (get (deref s) (deref i))
            let j = ref subi ((deref i) 1)
            recursive let tmp2 = lam ignore.
                modref
            tmp1 ()
        else
            ()
    in

    let ans = 
        if lti x 2 then x 
        else 
            tmp ();
            deref sum
    in
    
    ans
in
-- printLn (int2string (fib 0));
-- printLn (int2string (fib 1));
-- printLn (int2string (fib 2));
-- printLn (int2string (fib 3));
-- printLn (int2string (fib 4));
-- printLn (int2string (fib 5));
-- printLn (int2string (fib 6))

--print (int2string (fib 1000000000))
int2string((fib 10000000))
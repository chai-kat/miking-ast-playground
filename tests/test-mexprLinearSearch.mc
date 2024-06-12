include "mexpr/pprint.mc"

mexpr

recursive let linearSearch = lam arr. lam target. lam index.
    match (geqi index (length arr)) with true then
        -1
    else
        match (eqi (get arr index) target) with true then
        --match (get arr index) with target then
            index
        else
            linearSearch arr target (addi index 1)
in

printLn (int2string (linearSearch [1, 2, 3, 4] 3 0))

-- def linear_search(arr, target, index=0):
--     if index >= len(arr):
--         return -1
--     if arr[index] == target:
--         return index
--     return linear_search(arr, target, index + 1)

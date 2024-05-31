def insertion_sort(arr):
    n = len(arr)
    i = 1
    while i < n:
        key = arr[i]
        j = i - 1
        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1
        arr[j + 1] = key
        i += 1

# Example usage:
arr = [5, 2, 9, 1, 5, 6]
insertion_sort(arr)
print(arr)

mexpr
let selectionSort = funcdecl_
    [
        (nvardecl_ (nameNoSym "n") tyunknown_ (length_ (var_ "s"))),
        (nvardecl_ (nameNoSym "i") tyunknown_ (int_ 1)),
        (while_ (leqi_ (var_ "i") (var_ "n")) [
            (nvardecl_ (nameNoSym "key") tyunknown_ (get_ (var_ "s") (var_ "i"))),
            (nvardecl_ (nameNoSym "j") tyunknown_ (subi_ (var_ "i") (int_ 1))),
            (while_ (and_ (geqi_ (var_ "j") (int_ 0)) (gti_ (get_ (var_ "s") (var_ "j")) (var_ "key"))) [
                (varassign_ (nameNoSym "s") (set_ (var_ "s") (addi_ (var_ "j") (int_ 1)) (get_ (var_ "s") (var_ "j")))),
                (varassign_ (nameNoSym "j") (subi_ (var_ "j") (int_ 1)))
            ]),
            (varassign_ (nameNoSym "s") (set_ (var_ "s") (addi_ (var_ "j") (int_ 1)) (var_ "key"))),
            (varassign_ (nameNoSym "i") (addi_ (var_ "i") (int_ 1)))
        ]),
        return_ (var_ "s")
    ]

    tyunknown_

    [
        param_ (nameNoSym "s") tyunknown_
    ]
in
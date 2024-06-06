(* Insertion sort implementation in OCaml *)
let insertion_sort array =
    let rec insert_sorted element sorted_arr =
      match sorted_arr with
      | [] -> [element]
      | hd :: tl ->
        if element <= hd then
          element :: sorted_arr
        else
          hd :: (insert_sorted element tl)
    in
  
    let rec sort_recursively unsorted_arr sorted_arr =
      match unsorted_arr with
      | [] -> sorted_arr
      | hd :: tl -> sort_recursively tl (insert_sorted hd sorted_arr)
    in
  
    match (Array.length array) < 2 with
    | true -> array
    | false -> 
      let array_list = Array.to_list array in
      let sorted_list = sort_recursively (List.tl array_list) [List.hd array_list] in
      Array.of_list sorted_list
in
(* Function to fill a sequence with numbers from 0 to len - 1 *)
let rec fill_sequence s pos len =
if len = 0 then s
else fill_sequence (pos :: s) (pos + 1) (len - 1)
in
(* Create the sequence *)
let sequence = (fill_sequence [] 0 100000000) in
(* print_endline (String.concat " " (List.map string_of_int sequence)); *)

(* Convert the sequence list to an array *)
let sequence_array = Array.of_list sequence in

(* Uncomment the following line to sort the sequence using insertion sort *)
let sorted_sequence = insertion_sort sequence_array in
(* print_endline (String.concat " " (Array.to_list (Array.map string_of_int sorted_sequence))); *)
sorted_sequence

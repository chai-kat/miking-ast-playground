include "seq.mc"
include "mexpr/pprint.mc"

mexpr
    let forwards_direction = ["0","1","2"] in
    let dsjfslj = reverse [] in
    printLn (head dsjfslj);
    ()
    -- let y = map (lam x. (printLn x); ()) (forwards_direction) in
    -- map (lam x. (printLn x); ()) (reverse forwards_direction)
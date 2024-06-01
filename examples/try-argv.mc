include "arg.mc"
include "mexpr/pprint.mc"
mexpr

type Options = {
  n : Int
} in

let default = {
    n = 7
} in

let options = {
  args = tail argv,
  optionsStartWith = ["-"]
}

in

let config = [
  ([("-n", " ", "<value>")], lam p. { p.options with n = argToInt p })
]

in 

let result = argParse_general options default config in

let n = 
match result with ParseOK r 
then r.options.n 
else error "Failed to parse -n argument"
in


printLn (int2string n)
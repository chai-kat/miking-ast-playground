

int foo(x) {
  if (x == 5) {
    return 1
  }
  else {
    -- call external function? something else? else not necessary?
  } 
  print x
  
}



if ( x== 5) {
  return x
} else {
  x = x+2
}
let y = x
return y;

let result = match x with 5
then x
else modref x ((deref x) + 2)


-- since every lambda contains 1 expression, if for instance, the if statement is contained within 1 lambda, it needs to return the same 
-- type regardless of the branch taken, hence if given a case such as: (above), we need a continuation from the else branch to what is next
-- with the contents of the else branch as the argument
-- a continuation is required from a branch, only if there is no return statement in said branch -- meaning the if branch does not require one
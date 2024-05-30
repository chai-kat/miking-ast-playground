-- need to show that we avoid name collisions when we declare two functions in an AST

-- def foo(x):
--     y = x+3
--     return y

-- def bar(x):
--     y = x+5
--     return y

-- foo(5) = 
-- print deref y
-- bar(7)
-- print deref y

-- note for later: all variables technically static in functions, show that it doesn't have unintended side effects
-- global variables possible by normal derefs (library user's responsibilty to track these)
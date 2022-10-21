-- let expression

f : {a} -> a -> a
f = let { g = \x -> x; h = \x -> x } in g
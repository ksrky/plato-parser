-- empty block after layout keyword

f : {a} -> a -> a
f = let {} in \x -> x


g : {a} -> a -> Void
g x = case x of
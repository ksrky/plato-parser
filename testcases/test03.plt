module Plato.List;

data List a = Nil | (::) a (List a);

infixr 5 ::;

(++) : forall a. List a -> List a -> List a;
(++) l m = case l of {
    Nil -> m;
    x :: xs -> x :: (xs ++ m);
};

infixl 5 ++;

head : forall a. List a -> Maybe a;
head l = case l of {
    Nil -> Nothing;
    hd :: _ -> Just hd;
};

tail : forall a. List a -> List a;
tail l = case l of {
    Nil -> Nil;
    hd :: tl -> tl;
};

length : forall a. List a -> Nat;
length l = case l of {
    Nil -> Zero;
    x :: xs -> Succ (length xs);
};

map : forall a b. (a -> b) -> List a -> List b;
map f l = case l of {
    Nil -> Nil;
    x :: xs -> f x :: map f xs;
};

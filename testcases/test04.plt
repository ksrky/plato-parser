-- using common symbol as op

import Plato.Bool;

data Nat = Zero | Succ Nat;

(==) : Nat -> Nat -> Bool;
(==) m n = case m of {
    Succ m' -> case n of {
        Succ n' -> m' == n';
        Zero -> False;
    };
    Zero -> case n' of {
        Succ n' -> False;
        Zero -> True;
    };
};

infix 4 ==;

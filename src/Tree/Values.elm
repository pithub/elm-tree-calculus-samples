module Tree.Values exposing (..)

import Tree.Tree as T


fPower : T.Tree -> Int -> T.Tree -> T.Tree
fPower zero count tree =
    if count > 0 then
        fPower (T.add [ zero ] tree) (count - 1) tree

    else
        zero

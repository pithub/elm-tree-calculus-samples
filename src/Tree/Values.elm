module Tree.Values exposing (..)

import Tree.Tree as T



-- 3.2 Natural Trees


fPower : T.Tree -> Int -> T.Tree -> T.Tree
fPower zero count tree =
    if count > 0 then
        fPower (T.add [ zero ] tree) (count - 1) tree

    else
        zero



-- 3.3 Tree Calculus


cK : T.Tree
cK =
    T.delta [ T.delta [] ]


cI : T.Tree
cI =
    T.delta [ T.delta [ T.delta [] ], T.delta [ T.delta [] ] ]


cD : T.Tree
cD =
    T.delta [ T.delta [ T.delta [] ], T.delta [ T.delta [], T.delta [] ] ]


fD : T.Tree -> T.Tree
fD x =
    T.delta [ T.delta [ x ] ]


cS : T.Tree
cS =
    fD (cK |> T.add [ cD ]) |> T.add [ fD cK |> T.add [ cK |> T.add [ cD ] ] ]

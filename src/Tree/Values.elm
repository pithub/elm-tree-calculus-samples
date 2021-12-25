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



-- 3.5 Propositional Logic


cTrue : T.Tree
cTrue =
    cK


cFalse : T.Tree
cFalse =
    cK |> T.add [ cI ]


cAnd : T.Tree
cAnd =
    fD (cK |> T.add [ cK |> T.add [ cI ] ])


cOr : T.Tree
cOr =
    fD (cK |> T.add [ cK ]) |> T.add [ cI ]


cImplies : T.Tree
cImplies =
    fD (cK |> T.add [ cK ])


cNot : T.Tree
cNot =
    fD (cK |> T.add [ cK ]) |> T.add [ fD (cK |> T.add [ cK |> T.add [ cI ] ]) |> T.add [ cI ] ]


cIff : T.Tree
cIff =
    T.delta [ T.delta [ cI, cNot ], T.delta [] ]

module Tree.Values exposing (..)

import Tree.Tree as T



-- 3.2 Natural Trees


fPower : T.Tree -> Int -> T.Tree -> List T.Tree -> T.Tree
fPower zero count tree children =
    if count > 0 then
        fPower (T.add [ zero ] tree) (count - 1) tree children

    else
        zero |> T.add children



-- 3.3 Tree Calculus


cK : List T.Tree -> T.Tree
cK children =
    T.delta (T.delta [] :: children)


cI : List T.Tree -> T.Tree
cI children =
    T.delta (T.delta [ T.delta [] ] :: T.delta [ T.delta [] ] :: children)


cD : List T.Tree -> T.Tree
cD children =
    T.delta (T.delta [ T.delta [] ] :: T.delta [ T.delta [], T.delta [] ] :: children)


fD : T.Tree -> List T.Tree -> T.Tree
fD x children =
    T.delta (T.delta [ x ] :: children)


cS : List T.Tree -> T.Tree
cS children =
    fD (cK [ cD [] ]) (fD (cK []) [ cK [ cD [] ] ] :: children)



-- 3.5 Propositional Logic


cTrue : List T.Tree -> T.Tree
cTrue children =
    cK children


cFalse : List T.Tree -> T.Tree
cFalse children =
    cK (cI [] :: children)


cAnd : List T.Tree -> T.Tree
cAnd children =
    fD (cK [ cK [ cI [] ] ]) children


cOr : List T.Tree -> T.Tree
cOr children =
    fD (cK [ cK [] ]) (cI [] :: children)


cImplies : List T.Tree -> T.Tree
cImplies children =
    fD (cK [ cK [] ]) children


cNot : List T.Tree -> T.Tree
cNot children =
    fD (cK [ cK [] ]) (fD (cK [ cK [ cI [] ] ]) [ cI [] ] :: children)


cIff : List T.Tree -> T.Tree
cIff children =
    T.delta (T.delta [ cI [], cNot [] ] :: T.delta [] :: children)



-- 3.6 Pairs


cPair : List T.Tree -> T.Tree
cPair children =
    T.delta children


fFirst : T.Tree -> List T.Tree -> T.Tree
fFirst pair children =
    T.delta (pair :: T.delta [] :: cK [] :: children)


fSecond : T.Tree -> List T.Tree -> T.Tree
fSecond pair children =
    T.delta (pair :: T.delta [] :: cK [ cI [] ] :: children)



-- 3.7 Natural Numbers


vZero : T.Tree
vZero =
    T.delta []


cSuccessor : List T.Tree -> T.Tree
cSuccessor children =
    cK children


fNum : Int -> List T.Tree -> T.Tree
fNum n children =
    fPower vZero n (cSuccessor []) children


cIsZero : List T.Tree -> T.Tree
cIsZero children =
    fD (fPower (cI []) 4 (cK []) []) (fD (cK [ cK [] ]) [ T.delta [] ] :: children)


cPredecessor : List T.Tree -> T.Tree
cPredecessor children =
    fD (fPower (cI []) 2 (cK []) []) (fD (cK [ T.delta [] ]) [ T.delta [] ] :: children)



-- 3.8 Fundamental Queries


fQuery : T.Tree -> T.Tree -> T.Tree -> List T.Tree -> T.Tree
fQuery is0 is1 is2 children =
    fD (cK [ is1 ])
        (fD (fPower (cI []) 2 (cK []) [])
            [ fD (fPower is2 5 (cK []) [])
                [ fD (fPower is0 3 (cK []) [])
                    [ T.delta [] ]
                ]
            ]
            :: children
        )


cIsLeaf : List T.Tree -> T.Tree
cIsLeaf children =
    fQuery (cTrue []) (cFalse []) (cFalse []) children


cIsStem : List T.Tree -> T.Tree
cIsStem children =
    fQuery (cFalse []) (cTrue []) (cFalse []) children


cIsFork : List T.Tree -> T.Tree
cIsFork children =
    fQuery (cFalse []) (cFalse []) (cTrue []) children

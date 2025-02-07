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



-- 4.4 Waiting


fWait : T.Tree -> T.Tree -> List T.Tree -> T.Tree
fWait x y children =
    fD (cI []) (fD (cK [ y ]) [ cK [ x ] ] :: children)


fWait1 : T.Tree -> List T.Tree -> T.Tree
fWait1 x children =
    T.lambda "y" (fWait x (T.var "y" []) []) children


fWait2 : T.Tree -> T.Tree -> List T.Tree -> T.Tree
fWait2 x y children =
    T.lambda "z" (T.bind "w" (x |> T.add [ y, T.var "z" [], T.var "w" [] ]) []) children


fWait21 : T.Tree -> List T.Tree -> T.Tree
fWait21 x children =
    T.lambda "y" (fWait2 x (T.var "y" []) []) children


fWait3 : T.Tree -> T.Tree -> List T.Tree -> T.Tree
fWait3 x y children =
    T.lambda "z" (T.lambda "t" (T.bind "w" (x |> T.add [ y, T.var "z" [], T.var "t" [], T.var "w" [] ]) []) []) children


fWait31 : T.Tree -> List T.Tree -> T.Tree
fWait31 x children =
    T.lambda "y" (fWait3 x (T.var "y" []) []) children



-- 4.5 Fixpoint Functions


cSelfApply : List T.Tree -> T.Tree
cSelfApply children =
    fD (cI []) (cI [] :: children)


fZ : T.Tree -> List T.Tree -> T.Tree
fZ f children =
    fWait (cSelfApply []) (fD (fWait1 (cSelfApply []) []) [ cK [ f ] ]) children


fSwap : T.Tree -> List T.Tree -> T.Tree
fSwap f children =
    T.lambda "x" (T.lambda "y" (f |> T.add [ T.var "y" [], T.var "x" [] ]) []) children


fY2 : T.Tree -> List T.Tree -> T.Tree
fY2 f children =
    fZ (fSwap f []) children



-- 4.6 Arithmetic


cPlus : List T.Tree -> T.Tree
cPlus children =
    fY2
        (T.lambda "m"
            (T.lambda "r"
                (T.delta
                    [ T.var "m" []
                    , cI []
                    , cK [ T.lambda "x" (T.lambda "n" (T.var "r" [ T.var "x" [], cSuccessor [ T.var "n" [] ] ]) []) [] ]
                    ]
                )
                []
            )
            []
        )
        children


cSub : List T.Tree -> T.Tree
cSub children =
    fY2
        (T.lambda "m"
            (T.lambda "r"
                (T.delta
                    [ T.var "m" []
                    , cI []
                    , cK [ T.lambda "x" (T.lambda "n" (T.var "r" [ T.var "x" [], cPredecessor [ T.var "n" [] ] ]) []) [] ]
                    ]
                )
                []
            )
            []
        )
        children


cMul : List T.Tree -> T.Tree
cMul children =
    fY2
        (T.lambda "m"
            (T.lambda "r"
                (T.delta
                    [ T.var "m" []
                    , cK [ vZero ]
                    , cK [ T.lambda "x" (fD (T.var "r" [ T.var "x" [] ]) [ cPlus [] ]) [] ]
                    ]
                )
                []
            )
            []
        )
        children



-- 4.7 Lists and Strings


vNil : T.Tree
vNil =
    T.delta []


cCons : List T.Tree -> T.Tree
cCons children =
    T.delta children



-- 4.8 Mapping and Folding


cListMapSwap : List T.Tree -> T.Tree
cListMapSwap children =
    T.lambda "x"
        (T.delta
            [ T.var "x" []
            , cK [ cK [ vNil ] ]
            , T.lambda "h"
                (T.lambda "t"
                    (T.lambda "m"
                        (T.lambda "f"
                            (cCons
                                [ T.var "f" [ T.var "h" [] ]
                                , T.var "m" [ T.var "t" [], T.var "f" [] ]
                                ]
                            )
                            []
                        )
                        []
                    )
                    []
                )
                []
            ]
        )
        children


cListMap : List T.Tree -> T.Tree
cListMap children =
    fSwap (fY2 (cListMapSwap []) []) children


cListFoldLeftAux : List T.Tree -> T.Tree
cListFoldLeftAux children =
    T.lambda "y"
        (T.delta
            [ T.var "y" []
            , cK [ cK [ cI [] ] ]
            , T.lambda "h"
                (T.lambda "t"
                    (T.lambda "r"
                        (T.lambda "f"
                            (T.lambda "x"
                                (T.var "r" [ T.var "t" [], T.var "f" [], T.var "f" [ T.var "x" [], T.var "h" [] ] ])
                                []
                            )
                            []
                        )
                        []
                    )
                    []
                )
                []
            ]
        )
        children


cListFoldLeft : List T.Tree -> T.Tree
cListFoldLeft children =
    T.lambda "f"
        (T.lambda "x"
            (T.lambda "y"
                (fY2 (cListFoldLeftAux []) [ T.var "y" [], T.var "f" [], T.var "x" [] ])
                []
            )
            []
        )
        children


cListFoldRightAux : List T.Tree -> T.Tree
cListFoldRightAux children =
    T.lambda "y"
        (T.delta
            [ T.var "y" []
            , cK [ cK [ cI [] ] ]
            , T.lambda "h"
                (T.lambda "t"
                    (T.lambda "r"
                        (T.lambda "f"
                            (T.lambda "x"
                                (T.var "f" [ T.var "h" [], T.var "r" [ T.var "t" [], T.var "f" [], T.var "x" [] ] ])
                                []
                            )
                            []
                        )
                        []
                    )
                    []
                )
                []
            ]
        )
        children


cListFoldRight : List T.Tree -> T.Tree
cListFoldRight children =
    T.lambda "f"
        (T.lambda "x"
            (T.lambda "y"
                (fY2 (cListFoldRightAux []) [ T.var "y" [], T.var "f" [], T.var "x" [] ])
                []
            )
            []
        )
        children



-- 5.2 Size


cSize : List T.Tree -> T.Tree
cSize children =
    fY2
        (T.lambda "x"
            (cIsStem
                [ T.var "x" []
                , T.lambda "s"
                    (T.delta
                        [ T.var "x" [ T.delta [] ]
                        , T.delta []
                        , T.lambda "x1" (cK [ cSuccessor [ T.var "s" [ T.var "x1" [] ] ] ]) []
                        ]
                    )
                    []
                , T.delta
                    [ T.var "x" []
                    , cK [ cSuccessor [ vZero ] ]
                    , T.lambda "x1" (T.lambda "x2" (T.lambda "s" (cSuccessor [ cPlus [ T.var "s" [ T.var "x1" [] ], T.var "s" [ T.var "x2" [] ] ] ]) []) []) []
                    ]
                ]
            )
            []
        )
        children



-- 5.3 Equality


cEqual : List T.Tree -> T.Tree
cEqual children =
    fY2
        (T.lambda "x"
            (cIsStem
                [ T.var "x" []
                , T.lambda "e"
                    (T.lambda "y"
                        (cIsStem
                            [ T.var "y" []
                            , T.var "e"
                                [ T.var "x" [ T.delta [] ]
                                , T.var "y" [ T.delta [] ]
                                ]
                            , cFalse []
                            ]
                        )
                        []
                    )
                    []
                , T.delta
                    [ T.var "x" []
                    , T.lambda "e"
                        (T.lambda "y"
                            (cIsLeaf [ T.var "y" [] ])
                            []
                        )
                        []
                    , T.lambda "x1"
                        (T.lambda "x2"
                            (T.lambda "e"
                                (T.lambda "y"
                                    (cIsFork
                                        [ T.var "y" []
                                        , T.delta
                                            [ T.var "y" []
                                            , T.delta []
                                            , T.lambda "y1"
                                                (T.lambda "y2"
                                                    (T.var "e"
                                                        [ T.var "x1" []
                                                        , T.var "y1" []
                                                        , T.var "e" [ T.var "x2" [], T.var "y2" [] ]
                                                        , cFalse []
                                                        ]
                                                    )
                                                    []
                                                )
                                                []
                                            ]
                                        , cFalse []
                                        ]
                                    )
                                    []
                                )
                                []
                            )
                            []
                        )
                        []
                    ]
                ]
            )
            []
        )
        children



-- 5.4 Tagging


fTag : T.Tree -> T.Tree -> List T.Tree -> T.Tree
fTag t f children =
    fD t (fD f [ cK [ cK [] ] ] :: children)


cGetTag : List T.Tree -> T.Tree
cGetTag children =
    T.lambda "p" (fFirst (fFirst (T.var "p" []) [ T.delta [] ]) []) children


cUnTag : List T.Tree -> T.Tree
cUnTag children =
    T.lambda "x" (fFirst (fFirst (fSecond (T.var "x" []) []) [ T.delta [] ]) []) children


fTagWait : T.Tree -> List T.Tree -> T.Tree
fTagWait t children =
    T.lambda "w" (fTag t (fWait (cSelfApply []) (T.var "w" []) []) []) children


fY2t : T.Tree -> T.Tree -> List T.Tree -> T.Tree
fY2t t f children =
    fTag t (fWait (cSelfApply []) (fD (fTagWait t []) [ cK [ fSwap f [] ] ]) []) children



-- 5.5 Simple Types


tBool : T.Tree
tBool =
    T.delta [ vZero ]


tNat : T.Tree
tNat =
    T.delta [ cSuccessor [ vZero ] ]


tFun : T.Tree -> T.Tree -> T.Tree
tFun u t =
    T.delta [ u, t ]


tTyped : T.Tree -> T.Tree -> List T.Tree -> T.Tree
tTyped typ tree children =
    fTag typ tree children


tTrue : List T.Tree -> T.Tree
tTrue children =
    tTyped tBool (cTrue []) children


tFalse : List T.Tree -> T.Tree
tFalse children =
    tTyped tBool (cFalse []) children


tAnd : List T.Tree -> T.Tree
tAnd children =
    tTyped (tFun tBool (tFun tBool tBool)) (cAnd []) children


tOr : List T.Tree -> T.Tree
tOr children =
    tTyped (tFun tBool (tFun tBool tBool)) (cOr []) children


tImplies : List T.Tree -> T.Tree
tImplies children =
    tTyped (tFun tBool (tFun tBool tBool)) (cImplies []) children


tNot : List T.Tree -> T.Tree
tNot children =
    tTyped (tFun tBool tBool) (cNot []) children


tIff : List T.Tree -> T.Tree
tIff children =
    tTyped (tFun tBool (tFun tBool tBool)) (cIff []) children


tZero : List T.Tree -> T.Tree
tZero children =
    tTyped tNat vZero children


tSuccessor : List T.Tree -> T.Tree
tSuccessor children =
    tTyped (tFun tNat tNat) (cSuccessor []) children


tIsZero : List T.Tree -> T.Tree
tIsZero children =
    tTyped (tFun tNat tBool) (cIsZero []) children


tPlus : List T.Tree -> T.Tree
tPlus children =
    tTyped (tFun tNat (tFun tNat tNat)) (cPlus []) children


tError : List T.Tree -> T.Tree
tError children =
    T.delta children


tTypeCheck : List T.Tree -> T.Tree
tTypeCheck children =
    T.lambda "equal"
        (T.lambda "x"
            (T.lambda "y"
                (cIsFork
                    [ T.var "x" []
                    , T.delta
                        [ T.var "x" []
                        , T.delta []
                        , T.lambda "u"
                            (T.lambda "t"
                                (T.var "equal" [ T.var "u" [], T.var "y" [], T.var "t" [], tError [] ])
                                []
                            )
                            []
                        ]
                    , tError []
                    ]
                )
                []
            )
            []
        )
        (cEqual [] :: children)


tApp : List T.Tree -> T.Tree
tApp children =
    T.lambda "typeCheck"
        (T.lambda "getTag"
            (T.lambda "unTag"
                (T.lambda "f"
                    (T.lambda "x"
                        (fTag
                            (T.var "typeCheck" [ T.var "getTag" [ T.var "f" [] ], T.var "getTag" [ T.var "x" [] ] ])
                            (T.var "unTag" [ T.var "f" [], T.var "unTag" [ T.var "x" [] ] ])
                            []
                        )
                        []
                    )
                    []
                )
                []
            )
            []
        )
        (tTypeCheck [] :: cGetTag [] :: cUnTag [] :: children)

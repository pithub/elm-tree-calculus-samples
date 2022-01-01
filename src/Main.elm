module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Tree.Format as TF
import Tree.Tree as T
import Tree.Values as V



-- SAMPLES


samples : List ( String, List Sample )
samples =
    [ ( "3.2 Natural Trees"
      , [ ( "Δ", T.delta [] )
        , ( "ΔΔ", T.delta [ T.delta [] ] )
        , ( "Δ(ΔΔ)(ΔΔ)", T.delta [ T.delta [ T.delta [] ], T.delta [ T.delta [] ] ] )
        , ( "Δ⁵Δ", V.fPower (T.delta []) 5 (T.delta []) [] )
        , ( "x⁵Δ", V.fPower (T.delta []) 5 (T.var "x" []) [] )
        ]
      )
    , ( "3.3 Tree Calculus"
      , [ ( "ΔΔyz", T.delta [ T.delta [], T.var "y" [], T.var "z" [] ] )
        , ( "Δ(Δx)yz", T.delta [ T.delta [ T.var "x" [] ], T.var "y" [], T.var "z" [] ] )
        , ( "Δ(Δwx)yz", T.delta [ T.delta [ T.var "w" [], T.var "x" [] ], T.var "y" [], T.var "z" [] ] )
        , ( "K", V.cK [] )
        , ( "Kyz", V.cK [ T.var "y" [], T.var "z" [] ] )
        , ( "I", V.cI [] )
        , ( "Ix", V.cI [ T.var "x" [] ] )
        , ( "D", V.cD [] )
        , ( "Dxyz", V.cD [ T.var "x" [], T.var "y" [], T.var "z" [] ] )
        , ( "d{x}", V.fD (T.var "x" []) [] )
        , ( "S", V.cS [] )
        , ( "Sxyz", V.cS [ T.var "x" [], T.var "y" [], T.var "z" [] ] )
        ]
      )
    , ( "3.5 Propositional Logic"
      , [ ( "and", V.cAnd [] )
        , ( "and true x", V.cAnd [ V.cTrue [], T.var "x" [] ] )
        , ( "and false x", V.cAnd [ V.cFalse [], T.var "x" [] ] )
        , ( "or", V.cOr [] )
        , ( "or true x", V.cOr [ V.cTrue [], T.var "x" [] ] )
        , ( "or false x", V.cOr [ V.cFalse [], T.var "x" [] ] )
        , ( "implies", V.cImplies [] )
        , ( "implies true x", V.cImplies [ V.cTrue [], T.var "x" [] ] )
        , ( "implies false x", V.cImplies [ V.cFalse [], T.var "x" [] ] )
        , ( "not", V.cNot [] )
        , ( "not true", V.cNot [ V.cTrue [] ] )
        , ( "not false", V.cNot [ V.cFalse [] ] )
        , ( "iff", V.cIff [] )
        , ( "iff true x", V.cIff [ V.cTrue [], T.var "x" [] ] )
        , ( "iff false true", V.cIff [ V.cFalse [], V.cTrue [] ] )
        , ( "iff false false", V.cIff [ V.cFalse [], V.cFalse [] ] )
        ]
      )
    , ( "3.6 Pairs"
      , [ ( "pair x y", V.cPair [ T.var "x" [], T.var "y" [] ] )
        , ( "first{pair x y}", V.fFirst (V.cPair [ T.var "x" [], T.var "y" [] ]) [] )
        , ( "second{pair x y}", V.fSecond (V.cPair [ T.var "x" [], T.var "y" [] ]) [] )
        ]
      )
    , ( "3.7 Natural Numbers"
      , [ ( "zero", V.vZero )
        , ( "successor", V.cSuccessor [] )
        , ( "num 5", V.fNum 5 [] )
        , ( "isZero", V.cIsZero [] )
        , ( "isZero zero", V.cIsZero [ V.vZero ] )
        , ( "isZero (successor n)", V.cIsZero [ V.cSuccessor [ T.var "n" [] ] ] )
        , ( "predecessor", V.cPredecessor [] )
        , ( "predecessor zero", V.cPredecessor [ V.vZero ] )
        , ( "predecessor (successor n)", V.cPredecessor [ V.cSuccessor [ T.var "n" [] ] ] )
        ]
      )
    , ( "3.8 Fundamental Queries"
      , [ ( "query is0 is1 is2", V.fQuery (T.var "is0" []) (T.var "is1" []) (T.var "is2" []) [] )
        , ( "isLeaf", V.cIsLeaf [] )
        , ( "isLeaf Δ", V.cIsLeaf [ T.delta [] ] )
        , ( "isLeaf ΔΔ", V.cIsLeaf [ T.delta [ T.delta [] ] ] )
        , ( "isLeaf ΔΔΔ", V.cIsLeaf [ T.delta [ T.delta [], T.delta [] ] ] )
        , ( "isStem", V.cIsStem [] )
        , ( "isStem Δ", V.cIsStem [ T.delta [] ] )
        , ( "isStem ΔΔ", V.cIsStem [ T.delta [ T.delta [] ] ] )
        , ( "isStem ΔΔΔ", V.cIsStem [ T.delta [ T.delta [], T.delta [] ] ] )
        , ( "isFork", V.cIsFork [] )
        , ( "isFork Δ", V.cIsFork [ T.delta [] ] )
        , ( "isFork ΔΔ", V.cIsFork [ T.delta [ T.delta [] ] ] )
        , ( "isFork ΔΔΔ", V.cIsFork [ T.delta [ T.delta [], T.delta [] ] ] )
        ]
      )
    , ( "4.2 Variable Binding"
      , [ ( "[x]Kxx", T.bind "x" (V.cK [ T.var "x" [], T.var "x" [] ]) [] )
        , ( "([x]Kxx)u", T.bind "x" (V.cK [ T.var "x" [], T.var "x" [] ]) [ T.var "u" [] ] )
        , ( "[x]I", T.bind "x" (V.cI []) [] )
        , ( "([x]I)u", T.bind "x" (V.cI []) [ T.var "u" [] ] )
        , ( "λ*x.Kxx", T.lambda "λx" (V.cK [ T.var "x" [], T.var "x" [] ]) [] )
        , ( "(λ*x.Kxx)u", T.lambda "x" (V.cK [ T.var "x" [], T.var "x" [] ]) [ T.var "u" [] ] )
        , ( "λ*x.I", T.lambda "x" (V.cI []) [] )
        , ( "(λ*x.I)u", T.lambda "x" (V.cI []) [ T.var "u" [] ] )
        , ( "(λ*x.d{x})u", T.lambda "x" (V.fD (T.var "x" []) []) [ T.var "u" [] ] )
        , ( "(λ*p.first{p})(pair x y)", T.lambda "p" (V.fFirst (T.var "p" []) []) [ V.cPair [ T.var "x" [], T.var "y" [] ] ] )
        , ( "(λ*p.second{p})(pair x y)", T.lambda "p" (V.fSecond (T.var "p" []) []) [ V.cPair [ T.var "x" [], T.var "y" [] ] ] )
        ]
      )
    , ( "4.4 Waiting"
      , [ ( "λ*x.λ*y.λ*z.xyz", T.lambda "x" (T.lambda "y" (T.lambda "z" (T.var "x" [ T.var "y" [], T.var "z" [] ]) []) []) [] )
        , ( "λ*x.λ*y.[z]xyz", T.lambda "x" (T.lambda "y" (T.bind "z" (T.var "x" [ T.var "y" [], T.var "z" [] ]) []) []) [] )
        , ( "(λ*x.λ*y.λ*z.xyz)mn", T.lambda "x" (T.lambda "y" (T.lambda "z" (T.var "x" [ T.var "y" [], T.var "z" [] ]) []) []) [ T.var "m" [], T.var "n" [] ] )
        , ( "(λ*x.λ*y.[z]xyz)mn", T.lambda "x" (T.lambda "y" (T.bind "z" (T.var "x" [ T.var "y" [], T.var "z" [] ]) []) []) [ T.var "m" [], T.var "n" [] ] )
        , ( "wait{x,y}", V.fWait (T.var "x" []) (T.var "y" []) [] )
        , ( "wait{x,y}a", V.fWait (T.var "x" []) (T.var "y" []) [ T.var "a" [] ] )
        , ( "wait1{x}", V.fWait1 (T.var "x" []) [] )
        , ( "wait1{x}y", V.fWait1 (T.var "x" []) [ T.var "y" [] ] )
        , ( "wait1{x}ya", V.fWait1 (T.var "x" []) [ T.var "y" [], T.var "a" [] ] )
        , ( "wait2{x,y}", V.fWait2 (T.var "x" []) (T.var "y" []) [] )
        , ( "wait21{x}", V.fWait21 (T.var "x" []) [] )
        , ( "wait3{x,y}", V.fWait3 (T.var "x" []) (T.var "y" []) [] )
        , ( "wait31{x}", V.fWait31 (T.var "x" []) [] )
        , ( "wait3{x,y}a", V.fWait3 (T.var "x" []) (T.var "y" []) [ T.var "a" [] ] )
        , ( "wait3{x,y}ab", V.fWait3 (T.var "x" []) (T.var "y" []) [ T.var "a" [], T.var "b" [] ] )
        , ( "wait3{x,y}abc", V.fWait3 (T.var "x" []) (T.var "y" []) [ T.var "a" [], T.var "b" [], T.var "c" [] ] )
        ]
      )
    , ( "4.5 Fixpoint Functions"
      , [ ( "selfApply", V.cSelfApply [] )
        , ( "selfApply w", V.cSelfApply [ T.var "w" [] ] )
        , ( "Z{f}", V.fZ (T.var "f" []) [] )
        , ( "Z{f}x", V.fZ (T.var "f" []) [ T.var "x" [] ] )
        , ( "fSwap{f}xy", V.fSwap (T.var "f" []) [ T.var "x" [], T.var "y" [] ] )
        , ( "Y2{f}", V.fY2 (T.var "f" []) [] )
        , ( "Y2{f}x", V.fY2 (T.var "f" []) [ T.var "x" [] ] )
        , ( "(λ*f.Y2{f})f", T.lambda "f" (V.fY2 (T.var "f" []) []) [ T.var "f" [] ] )
        ]
      )
    , ( "4.6 Arithmetic"
      , [ ( "plus", V.cPlus [] )
        , ( "plus 2", V.cPlus [ V.fNum 2 [] ] )
        , ( "plus 2 3", V.cPlus [ V.fNum 2 [], V.fNum 3 [] ] )
        , ( "plus 4", V.cPlus [ V.fNum 4 [] ] )
        , ( "plus 4 1", V.cPlus [ V.fNum 4 [], V.fNum 1 [] ] )
        , ( "sub", V.cSub [] )
        , ( "sub 2", V.cSub [ V.fNum 2 [] ] )
        , ( "sub 2 5", V.cSub [ V.fNum 2 [], V.fNum 5 [] ] )
        , ( "mul", V.cMul [] )
        , ( "mul 0", V.cMul [ V.fNum 0 [] ] )
        , ( "mul 0 3", V.cMul [ V.fNum 0 [], V.fNum 3 [] ] )
        , ( "mul 2", V.cMul [ V.fNum 2 [] ] )
        , ( "mul 2 3", V.cMul [ V.fNum 2 [], V.fNum 3 [] ] )
        ]
      )
    , ( "4.7 Lists and Strings"
      , [ ( "a::b::c::nil", V.cCons [ T.var "a" [], V.cCons [ T.var "b" [], V.cCons [ T.var "c" [], V.vNil ] ] ] )
        ]
      )
    , ( "4.8 Mapping and Folding"
      , [ ( "listMap", V.fSwap (T.var "Y2" [ V.cListMapSwap [] ]) [] )
        , ( "listMap f nil", V.cListMap [ T.var "f" [], V.vNil ] )
        , ( "listMap f a::b::c::nil", V.cListMap [ T.var "f" [], V.cCons [ T.var "a" [], V.cCons [ T.var "b" [], V.cCons [ T.var "c" [], V.vNil ] ] ] ] )
        , ( "liftFoldLeft", T.lambda "f" (T.lambda "x" (T.lambda "y" (T.var "Y2" [ V.cListFoldLeftAux [], T.var "y" [], T.var "f" [], T.var "x" [] ]) []) []) [] )
        , ( "liftFoldLeft f x nil", V.cListFoldLeft [ T.var "f" [], T.var "x" [], V.vNil ] )
        , ( "liftFoldLeft f x a::b::c::nil", V.cListFoldLeft [ T.var "f" [], T.var "x" [], V.cCons [ T.var "a" [], V.cCons [ T.var "b" [], V.cCons [ T.var "c" [], V.vNil ] ] ] ] )
        , ( "liftFoldRight", T.lambda "f" (T.lambda "x" (T.lambda "y" (T.var "Y2" [ V.cListFoldRightAux [], T.var "y" [], T.var "f" [], T.var "x" [] ]) []) []) [] )
        , ( "liftFoldRight f x nil", V.cListFoldRight [ T.var "f" [], T.var "x" [], V.vNil ] )
        , ( "liftFoldRight f x a::b::c::nil", V.cListFoldRight [ T.var "f" [], T.var "x" [], V.cCons [ T.var "a" [], V.cCons [ T.var "b" [], V.cCons [ T.var "c" [], V.vNil ] ] ] ] )
        ]
      )
    , ( "5.2 Size"
      , [ ( "size", T.var "Y2" [ T.lambda "x" (T.var "isStem" [ T.var "x" [], T.lambda "s" (T.delta [ T.var "x" [ T.delta [] ], T.delta [], T.lambda "x1" (V.cK [ V.cSuccessor [ T.var "s" [ T.var "x1" [] ] ] ]) [] ]) [], T.delta [ T.var "x" [], V.cK [ V.cSuccessor [ V.vZero ] ], T.lambda "x1" (T.lambda "x2" (T.lambda "s" (V.cSuccessor [ T.var "plus" [ T.var "s" [ T.var "x1" [] ], T.var "s" [ T.var "x2" [] ] ] ]) []) []) [] ] ]) [] ] )
        , ( "size K", V.cSize [ V.cK [] ] )
        , ( "size I", V.cSize [ V.cI [] ] )
        , ( "size D", V.cSize [ V.cD [] ] )
        , ( "size isLeaf", V.cSize [ V.cIsLeaf [] ] )
        ]
      )
    , ( "5.3 Equality"
      , [ ( "equal", T.var "Y2" [ T.lambda "x" (T.var "isStem" [ T.var "x" [], T.lambda "e" (T.lambda "y" (T.var "isStem" [ T.var "y" [], T.var "e" [ T.var "x" [ T.delta [] ], T.var "y" [ T.delta [] ] ], V.cFalse [] ]) []) [], T.delta [ T.var "x" [], T.lambda "e" (T.lambda "y" (T.var "isLeaf" [ T.var "y" [] ]) []) [], T.lambda "x1" (T.lambda "x2" (T.lambda "e" (T.lambda "y" (T.var "isFork" [ T.var "y" [], T.delta [ T.var "y" [], T.delta [], T.lambda "y1" (T.lambda "y2" (T.var "e" [ T.var "x1" [], T.var "y1" [], T.var "e" [ T.var "x2" [], T.var "y2" [] ], V.cFalse [] ]) []) [] ], V.cFalse [] ]) []) []) []) [] ] ]) [] ] )
        , ( "equal Δ Δ", V.cEqual [ T.delta [], T.delta [] ] )
        , ( "equal Δ K", V.cEqual [ T.delta [], V.cK [] ] )
        , ( "equal Δ 1", V.cEqual [ T.delta [], V.cSuccessor [ V.vZero ] ] )
        , ( "equal Δ I", V.cEqual [ T.delta [], V.cI [] ] )
        , ( "equal K Δ", V.cEqual [ V.cK [], T.delta [] ] )
        , ( "equal K K", V.cEqual [ V.cK [], V.cK [] ] )
        , ( "equal K 1", V.cEqual [ V.cK [], V.cSuccessor [ V.vZero ] ] )
        , ( "equal K I", V.cEqual [ V.cK [], V.cI [] ] )
        , ( "equal 1 Δ", V.cEqual [ V.cSuccessor [ V.vZero ], T.delta [] ] )
        , ( "equal 1 K", V.cEqual [ V.cSuccessor [ V.vZero ], V.cK [] ] )
        , ( "equal 1 1", V.cEqual [ V.cSuccessor [ V.vZero ], V.cSuccessor [ V.vZero ] ] )
        , ( "equal 1 K", V.cEqual [ V.cSuccessor [ V.vZero ], V.cI [] ] )
        , ( "equal I Δ", V.cEqual [ V.cI [], T.delta [] ] )
        , ( "equal I K", V.cEqual [ V.cI [], V.cK [] ] )
        , ( "equal I 1", V.cEqual [ V.cI [], V.cSuccessor [ V.vZero ] ] )
        , ( "equal I I", V.cEqual [ V.cI [], V.cI [] ] )
        ]
      )
    , ( "5.4 Tagging"
      , [ ( "tag t f", V.fTag (T.var "t" []) (T.var "f" []) [] )
        , ( "getTag", V.cGetTag [] )
        , ( "unTag", V.cUnTag [] )
        , ( "tag{\"true\",true} x y", V.fTag (T.var "true" []) (V.cTrue []) [ T.var "x" [], T.var "y" [] ] )
        , ( "getTag tag{\"true\",true}", V.cGetTag [ V.fTag (T.var "true" []) (V.cTrue []) [] ] )
        , ( "unTag tag{\"true\",true}", V.cUnTag [ V.fTag (T.var "true" []) (V.cTrue []) [] ] )
        , ( "Y2t{t,f}x", V.fY2t (T.var "t" []) (T.var "f" []) [ T.var "x" [] ] )
        , ( "getTag Y2t{t,f}", V.cGetTag [ V.fY2t (T.var "t" []) (T.var "f" []) [] ] )
        ]
      )
    ]



-- PROGRAM


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { sample : Sample
    , expanded : Bool
    }


type Msg
    = SetSample Sample
    | ToggleExpanded


type alias Sample =
    ( String, T.Tree )


init : Model
init =
    { sample = ( "Initial Sample", T.delta [] )
    , expanded = False
    }


view : Model -> H.Html Msg
view model =
    H.div
        [ HA.style "display" "flex"
        , HA.style "gap" "20px"
        , HA.style "margin" "0 20px"
        , HA.style "height" "100vh"
        , HA.style "font-family" "monospace"
        ]
        [ viewAllSamples
        , viewSelectedSample model
        ]


viewAllSamples : H.Html Msg
viewAllSamples =
    column []
        (List.concatMap
            (\( title, group ) -> H.h3 [] [ H.text title ] :: List.map viewNavSample group)
            samples
        )


viewNavSample : Sample -> H.Html Msg
viewNavSample sample =
    H.div [ HE.onClick (SetSample sample) ] [ H.text (Tuple.first sample) ]


viewSelectedSample : Model -> H.Html Msg
viewSelectedSample model =
    case model.sample of
        ( name, tree ) ->
            column [ HA.style "flex" "1" ]
                (H.h3 [] [ H.text name ]
                    :: (stepsToShow model.expanded tree
                            |> List.indexedMap
                                (\idx step ->
                                    H.pre
                                        (attributesForStep idx)
                                        (TF.toStrings step |> List.map (\line -> H.div [] [ H.text line ]))
                                )
                       )
                )


stepsToShow : Bool -> T.Tree -> List T.Tree
stepsToShow expanded tree =
    if expanded then
        T.evalSteps tree

    else
        List.filterMap identity [ Just tree, T.eval tree ]


attributesForStep : Int -> List (H.Attribute Msg)
attributesForStep idx =
    if idx > 0 then
        [ HE.onClick ToggleExpanded ]

    else
        []


column : List (H.Attribute m) -> List (H.Html m) -> H.Html m
column attributes children =
    H.div
        (List.append
            [ HA.style "height" "100%"
            , HA.style "margin" "0"
            , HA.style "overflow-y" "auto"
            ]
            attributes
        )
        children


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSample sample ->
            { model | sample = sample }

        ToggleExpanded ->
            { model | expanded = not model.expanded }

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

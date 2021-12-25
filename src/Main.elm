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
        , ( "Δ⁵Δ", V.fPower (T.delta []) 5 (T.delta []) )
        , ( "x⁵Δ", V.fPower (T.delta []) 5 (T.var "x" []) )
        ]
      )
    , ( "3.3 Tree Calculus"
      , [ ( "ΔΔyz", T.delta [ T.delta [], T.var "y" [], T.var "z" [] ] )
        , ( "Δ(Δx)yz", T.delta [ T.delta [ T.var "x" [] ], T.var "y" [], T.var "z" [] ] )
        , ( "Δ(Δwx)yz", T.delta [ T.delta [ T.var "w" [], T.var "x" [] ], T.var "y" [], T.var "z" [] ] )
        , ( "K", V.cK )
        , ( "Kyz", V.cK |> T.add [ T.var "y" [], T.var "z" [] ] )
        , ( "I", V.cI )
        , ( "Ix", V.cI |> T.add [ T.var "x" [] ] )
        , ( "D", V.cD )
        , ( "Dxyz", V.cD |> T.add [ T.var "x" [], T.var "y" [], T.var "z" [] ] )
        , ( "d{x}", V.fD (T.var "x" []) )
        , ( "S", V.cS )
        , ( "Sxyz", V.cS |> T.add [ T.var "x" [], T.var "y" [], T.var "z" [] ] )
        ]
      )
    , ( "3.5 Propositional Logic"
      , [ ( "and", V.cAnd )
        , ( "and true x", V.cAnd |> T.add [ V.cTrue, T.var "x" [] ] )
        , ( "and false x", V.cAnd |> T.add [ V.cFalse, T.var "x" [] ] )
        , ( "or", V.cOr )
        , ( "or true x", V.cOr |> T.add [ V.cTrue, T.var "x" [] ] )
        , ( "or false x", V.cOr |> T.add [ V.cFalse, T.var "x" [] ] )
        , ( "implies", V.cImplies )
        , ( "implies true x", V.cImplies |> T.add [ V.cTrue, T.var "x" [] ] )
        , ( "implies false x", V.cImplies |> T.add [ V.cFalse, T.var "x" [] ] )
        , ( "not", V.cNot )
        , ( "not true", V.cNot |> T.add [ V.cTrue ] )
        , ( "not false", V.cNot |> T.add [ V.cFalse ] )
        , ( "iff", V.cIff )
        , ( "iff true x", V.cIff |> T.add [ V.cTrue, T.var "x" [] ] )
        , ( "iff false true", V.cIff |> T.add [ V.cFalse, V.cTrue ] )
        , ( "iff false false", V.cIff |> T.add [ V.cFalse, V.cFalse ] )
        ]
      )
    ]



-- PROGRAM


main : Program () Sample Sample
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Sample =
    ( String, T.Tree )


init : Sample
init =
    ( "Initial Sample", T.delta [] )


view : Sample -> H.Html Sample
view sample =
    H.div
        [ HA.style "display" "flex"
        , HA.style "gap" "20px"
        , HA.style "margin" "0 20px"
        , HA.style "height" "100vh"
        , HA.style "font-family" "monospace"
        ]
        [ viewAllSamples
        , viewSelectedSample sample
        ]


viewAllSamples : H.Html Sample
viewAllSamples =
    column []
        (List.concatMap
            (\( title, group ) -> H.h3 [] [ H.text title ] :: List.map viewNavSample group)
            samples
        )


viewNavSample : Sample -> H.Html Sample
viewNavSample sample =
    H.div [ HE.onClick sample ] [ H.text (Tuple.first sample) ]


viewSelectedSample : Sample -> H.Html Sample
viewSelectedSample ( name, tree ) =
    column [ HA.style "flex" "1" ]
        (H.h3 [] [ H.text name ]
            :: (T.evalSteps tree
                    |> List.map
                        (\step ->
                            H.pre [] (TF.toStrings step |> List.map (\line -> H.div [] [ H.text line ]))
                        )
               )
        )


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


update : Sample -> Sample -> Sample
update selectedSample _ =
    selectedSample

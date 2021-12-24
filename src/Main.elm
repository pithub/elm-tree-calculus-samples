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
        [ H.h3 [] [ H.text name ]
        , H.pre [] (TF.toStrings tree |> List.map (\line -> H.div [] [ H.text line ]))
        ]


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

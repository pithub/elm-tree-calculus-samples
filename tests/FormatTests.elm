module FormatTests exposing (suite)

import Expect as E
import Test as E
import Tree.Format as F
import Tree.Tree as T


suite : E.Test
suite =
    let
        test : String -> T.Tree -> List String -> E.Test
        test description tree expected =
            E.test description <| \_ -> F.toStrings tree |> E.equal expected
    in
    E.describe "format tests"
        [ test "Δ" (T.delta []) <|
            [ "Δ"
            ]
        , test "x" (T.var "x" []) <|
            [ "x"
            ]
        , test "top" (T.var "top" []) <|
            [ "top"
            ]
        , test "ΔΔ" (T.delta [ T.delta [] ]) <|
            [ "Δ"
            , "Δ"
            ]
        , test "Δx" (T.delta [ T.var "x" [] ]) <|
            [ "Δ"
            , "x"
            ]
        , test "Δ bottom" (T.delta [ T.var "bottom" [] ]) <|
            [ "Δ     "
            , "bottom"
            ]
        , test "xΔ" (T.var "x" [ T.delta [] ]) <|
            [ "x─┐"
            , "  Δ"
            ]
        , test "top Δ" (T.var "top" [ T.delta [] ]) <|
            [ "top─┐"
            , "    Δ"
            ]
        , test "xy" (T.var "x" [ T.var "y" [] ]) <|
            [ "x─┐"
            , "  y"
            ]
        , test "top bottom" (T.var "top" [ T.var "bottom" [] ]) <|
            [ "top─┐     "
            , "    bottom"
            ]
        , test "ΔΔΔ" (T.delta [ T.delta [], T.delta [] ]) <|
            [ "Δ─┐"
            , "Δ Δ"
            ]
        , test "xΔΔ" (T.var "x" [ T.delta [], T.delta [] ]) <|
            [ "x─┬─┐"
            , "  Δ Δ"
            ]
        , test "xyz" (T.var "x" [ T.var "y" [], T.var "z" [] ]) <|
            [ "x─┬─┐"
            , "  y z"
            ]
        , test "top left right" (T.var "top" [ T.var "left" [], T.var "right" [] ]) <|
            [ "top─┬────┐    "
            , "    left right"
            ]
        , test "Δ(ΔΔ)" (T.delta [ T.delta [ T.delta [] ] ]) <|
            [ "Δ"
            , "Δ"
            , "Δ"
            ]
        , test "x(yz)" (T.var "x" [ T.var "y" [ T.var "z" [] ] ]) <|
            [ "x─┐  "
            , "  y─┐"
            , "    z"
            ]
        , test "top (middle bottom)" (T.var "top" [ T.var "middle" [ T.var "bottom" [] ] ]) <|
            [ "top─┐            "
            , "    middle─┐     "
            , "           bottom"
            ]
        , test "Δ(ΔΔ)Δ" (T.delta [ T.delta [ T.delta [] ], T.delta [] ]) <|
            [ "Δ─┐"
            , "Δ Δ"
            , "Δ  "
            ]
        , test "ΔΔ(ΔΔ)" (T.delta [ T.delta [], T.delta [ T.delta [] ] ]) <|
            [ "Δ─┐"
            , "Δ Δ"
            , "  Δ"
            ]
        , test "top (left bottom) right" (T.var "top" [ T.var "left" [ T.var "bottom" [] ], T.var "right" [] ]) <|
            [ "top─┬───────────┐    "
            , "    left─┐      right"
            , "         bottom      "
            ]
        ]

module BindTests exposing (suite)

import Expect as E
import Test as E
import Tree.Tree as T
import Tree.Values as V


suite : E.Test
suite =
    E.describe "bind tests"
        [ -- bind
          let
            test : String -> T.Tree -> T.Tree -> E.Test
            test description expected tree =
                E.test description <| \_ -> T.bind "x" tree [] |> E.equal expected
          in
          E.describe "bind"
            [ test "[x]x"
                (V.cI [])
                (T.var "x" [])
            , test "[x]y"
                (V.cK [ T.var "y" [] ])
                (T.var "y" [])
            , test "[x]Δ"
                (V.cK [ T.delta [] ])
                (T.delta [])
            , test "[x]ab"
                (V.fD (V.cK [ T.var "b" [] ]) [ V.cK [ T.var "a" [] ] ])
                (T.var "a" [ T.var "b" [] ])
            , test "[x]xb"
                (V.fD (V.cK [ T.var "b" [] ]) [ V.cI [] ])
                (T.var "x" [ T.var "b" [] ])
            , test "[x]ax"
                (V.fD (V.cI []) [ V.cK [ T.var "a" [] ] ])
                (T.var "a" [ T.var "x" [] ])
            , test "[x]xx"
                (V.fD (V.cI []) [ V.cI [] ])
                (T.var "x" [ T.var "x" [] ])
            , test "[x]abc"
                (V.fD (V.cK [ T.var "c" [] ]) [ V.fD (V.cK [ T.var "b" [] ]) [ V.cK [ T.var "a" [] ] ] ])
                (T.var "a" [ T.var "b" [], T.var "c" [] ])
            , test "[x]abcd"
                (V.fD (V.cK [ T.var "d" [] ]) [ V.fD (V.cK [ T.var "c" [] ]) [ V.fD (V.cK [ T.var "b" [] ]) [ V.cK [ T.var "a" [] ] ] ] ])
                (T.var "a" [ T.var "b" [], T.var "c" [], T.var "d" [] ])
            ]
        , -- bind and apply
          let
            test : String -> T.Tree -> T.Tree -> E.Test
            test description expected tree =
                E.test description <| \_ -> T.evaled (T.bind "x" tree [ T.var "u" [] ]) |> E.equal expected
          in
          E.describe "bind and apply"
            [ test "([x]x)u"
                (T.var "u" [])
                (T.var "x" [])
            , test "([x]y)u"
                (T.var "y" [])
                (T.var "y" [])
            , test "([x]Δ)u"
                (T.delta [])
                (T.delta [])
            , test "([x]ab)u"
                (T.var "a" [ T.var "b" [] ])
                (T.var "a" [ T.var "b" [] ])
            , test "([x]xb)u"
                (T.var "u" [ T.var "b" [] ])
                (T.var "x" [ T.var "b" [] ])
            , test "([x]ax)u"
                (T.var "a" [ T.var "u" [] ])
                (T.var "a" [ T.var "x" [] ])
            , test "([x]xx)u"
                (T.var "u" [ T.var "u" [] ])
                (T.var "x" [ T.var "x" [] ])
            , test "([x]abc)u"
                (T.var "a" [ T.var "b" [], T.var "c" [] ])
                (T.var "a" [ T.var "b" [], T.var "c" [] ])
            , test "([x]abcd)u"
                (T.var "a" [ T.var "b" [], T.var "c" [], T.var "d" [] ])
                (T.var "a" [ T.var "b" [], T.var "c" [], T.var "d" [] ])
            , test "([x]K)u"
                (V.cK [])
                (V.cK [])
            , test "([x]Kx)u"
                (V.cK [ T.var "u" [] ])
                (V.cK [ T.var "x" [] ])
            , test "([x]Kxx)u"
                (T.var "u" [])
                (V.cK [ T.var "x" [], T.var "x" [] ])
            ]
        ]

module EvalTests exposing (suite)

import Expect as E
import Test as E
import Tree.Tree as T
import Tree.Values as V


suite : E.Test
suite =
    E.describe "eval tests"
        [ -- evalRule
          let
            test : String -> Maybe T.Tree -> T.Tree -> E.Test
            test description expected tree =
                E.test description <| \_ -> T.evalRule tree |> E.equal expected
          in
          E.describe "evalRule"
            [ test "ΔΔyz"
                (Just (T.var "y" []))
                (T.delta [ T.delta [], T.var "y" [], T.var "z" [] ])
            , test "Δ(Δx)yz"
                (Just (T.var "y" [ T.var "z" [], T.var "x" [ T.var "z" [] ] ]))
                (T.delta [ T.delta [ T.var "x" [] ], T.var "y" [], T.var "z" [] ])
            , test "Δ(Δwx)yz"
                (Just (T.var "z" [ T.var "w" [], T.var "x" [] ]))
                (T.delta [ T.delta [ T.var "w" [], T.var "x" [] ], T.var "y" [], T.var "z" [] ])
            , test "Δxyz"
                Nothing
                (T.delta [ T.var "x" [], T.var "y" [], T.var "z" [] ])
            , test "Δ(Δvwx)yz"
                Nothing
                (T.delta [ T.delta [ T.var "v" [], T.var "w" [], T.var "x" [] ], T.var "y" [], T.var "z" [] ])
            , test "ΔΔyzab"
                (Just (T.var "y" [ T.var "a" [], T.var "b" [] ]))
                (T.delta [ T.delta [], T.var "y" [], T.var "z" [], T.var "a" [], T.var "b" [] ])
            , test "Δ(Δx)yzab"
                (Just (T.var "y" [ T.var "z" [], T.var "x" [ T.var "z" [] ], T.var "a" [], T.var "b" [] ]))
                (T.delta [ T.delta [ T.var "x" [] ], T.var "y" [], T.var "z" [], T.var "a" [], T.var "b" [] ])
            , test "Δ(Δwx)yzab"
                (Just (T.var "z" [ T.var "w" [], T.var "x" [], T.var "a" [], T.var "b" [] ]))
                (T.delta [ T.delta [ T.var "w" [], T.var "x" [] ], T.var "y" [], T.var "z" [], T.var "a" [], T.var "b" [] ])
            , test "Kyz.1"
                (Just (T.var "y" []))
                (V.cK |> T.add [ T.var "y" [], T.var "z" [] ])
            , test "Ix.1"
                (Just (T.delta [ T.delta [], T.var "x" [], T.delta [ T.var "x" [] ] ]))
                (V.cI |> T.add [ T.var "x" [] ])
            , test "Ix.2"
                (Just (T.var "x" []))
                (T.delta [ T.delta [], T.var "x" [], T.delta [ T.var "x" [] ] ])
            , test "Dxyz.1"
                (Just (T.delta [ T.delta [], T.delta [], T.var "x" [], T.delta [ T.var "x" [] ], T.var "y" [], T.var "z" [] ]))
                (V.cD |> T.add [ T.var "x" [], T.var "y" [], T.var "z" [] ])
            , test "Dxyz.2"
                (Just (T.delta [ T.delta [ T.var "x" [] ], T.var "y" [], T.var "z" [] ]))
                (T.delta [ T.delta [], T.delta [], T.var "x" [], T.delta [ T.var "x" [] ], T.var "y" [], T.var "z" [] ])
            , test "Dxyz.3"
                (Just (T.var "y" [ T.var "z" [], T.var "x" [ T.var "z" [] ] ]))
                (T.delta [ T.delta [ T.var "x" [] ], T.var "y" [], T.var "z" [] ])
            ]
        , -- evalStep
          let
            test : String -> Maybe T.Tree -> T.Tree -> E.Test
            test description expected tree =
                E.test description <| \_ -> T.evalStep tree |> E.equal expected
          in
          E.describe "evalStep"
            [ test "Δ(ΔΔyz)ab"
                (Just (T.delta [ T.var "y" [], T.var "a" [], T.var "b" [] ]))
                (T.delta [ T.delta [ T.delta [], T.var "y" [], T.var "z" [] ], T.var "a" [], T.var "b" [] ])
            , test "Δa(ΔΔyz)b"
                (Just (T.delta [ T.var "a" [], T.var "y" [], T.var "b" [] ]))
                (T.delta [ T.var "a" [], T.delta [ T.delta [], T.var "y" [], T.var "z" [] ], T.var "b" [] ])
            , test "Δab(ΔΔyz)"
                (Just (T.delta [ T.var "a" [], T.var "b" [], T.var "y" [] ]))
                (T.delta [ T.var "a" [], T.var "b" [], T.delta [ T.delta [], T.var "y" [], T.var "z" [] ] ])
            , test "Δ(ΔΔyz)(ΔΔab).1"
                (Just (T.delta [ T.var "y" [], T.delta [ T.delta [], T.var "a" [], T.var "b" [] ] ]))
                (T.delta [ T.delta [ T.delta [], T.var "y" [], T.var "z" [] ], T.delta [ T.delta [], T.var "a" [], T.var "b" [] ] ])
            , test "Δ(ΔΔyz)(ΔΔab).2"
                (Just (T.delta [ T.var "y" [], T.var "a" [] ]))
                (T.delta [ T.var "y" [], T.delta [ T.delta [], T.var "a" [], T.var "b" [] ] ])
            ]
        , -- evalSteps
          let
            test : String -> List T.Tree -> T.Tree -> E.Test
            test description expected tree =
                E.test description <| \_ -> T.evalSteps tree |> E.equal expected
          in
          E.describe "evalSteps"
            [ test "Δ(ΔΔyz)(ΔΔab)"
                [ T.delta [ T.delta [ T.delta [], T.var "y" [], T.var "z" [] ], T.delta [ T.delta [], T.var "a" [], T.var "b" [] ] ]
                , T.delta [ T.var "y" [], T.delta [ T.delta [], T.var "a" [], T.var "b" [] ] ]
                , T.delta [ T.var "y" [], T.var "a" [] ]
                ]
                (T.delta [ T.delta [ T.delta [], T.var "y" [], T.var "z" [] ], T.delta [ T.delta [], T.var "a" [], T.var "b" [] ] ])
            , test "Δ(Δ(ΔΔy))ab"
                [ T.delta [ T.delta [ T.delta [ T.delta [], T.var "y" [] ] ], T.var "a" [], T.var "b" [] ]
                , T.var "a" [ T.var "b" [], T.delta [ T.delta [], T.var "y" [], T.var "b" [] ] ]
                , T.var "a" [ T.var "b" [], T.var "y" [] ]
                ]
                (T.delta [ T.delta [ T.delta [ T.delta [], T.var "y" [] ] ], T.var "a" [], T.var "b" [] ])
            ]
        , -- eval
          let
            test : String -> Maybe T.Tree -> T.Tree -> E.Test
            test description expected tree =
                E.test description <| \_ -> T.eval tree |> E.equal expected
          in
          E.describe "eval"
            [ test "Δ(ΔΔyz)(ΔΔab)"
                (Just (T.delta [ T.var "y" [], T.var "a" [] ]))
                (T.delta [ T.delta [ T.delta [], T.var "y" [], T.var "z" [] ], T.delta [ T.delta [], T.var "a" [], T.var "b" [] ] ])
            , test "Δ(Δ(ΔΔy))ab"
                (Just (T.var "a" [ T.var "b" [], T.var "y" [] ]))
                (T.delta [ T.delta [ T.delta [ T.delta [], T.var "y" [] ] ], T.var "a" [], T.var "b" [] ])
            , test "Kyz"
                (Just (T.var "y" []))
                (V.cK |> T.add [ T.var "y" [], T.var "z" [] ])
            , test "Ix"
                (Just (T.var "x" []))
                (V.cI |> T.add [ T.var "x" [] ])
            , test "Dxyz"
                (Just (T.var "y" [ T.var "z" [], T.var "x" [ T.var "z" [] ] ]))
                (V.cD |> T.add [ T.var "x" [], T.var "y" [], T.var "z" [] ])
            ]
        ]

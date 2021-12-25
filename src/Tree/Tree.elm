module Tree.Tree exposing (Node(..), Tree(..), add, delta, eval, evalRule, evalStep, evalSteps, var)

import Tree.Util as Util



-- 3.2 Natural Trees


type Tree
    = Tree Node (List Tree)


type Node
    = Delta
    | Var String


delta : List Tree -> Tree
delta children =
    Tree Delta children


var : String -> List Tree -> Tree
var label children =
    Tree (Var label) children


add : List Tree -> Tree -> Tree
add new (Tree node children) =
    Tree node (List.append children new)



-- 3.3 Tree Calculus


evalRule : Tree -> Maybe Tree
evalRule tree =
    case tree of
        Tree Delta ((Tree Delta []) :: y :: _ :: rest) ->
            Just (y |> add rest)

        Tree Delta ((Tree Delta [ x ]) :: y :: z :: rest) ->
            Just (y |> add [ z, x |> add [ z ] ] |> add rest)

        Tree Delta ((Tree Delta [ w, x ]) :: _ :: z :: rest) ->
            Just (z |> add [ w, x ] |> add rest)

        _ ->
            Nothing


evalStep : Tree -> Maybe Tree
evalStep ((Tree _ rootChildren) as root) =
    let
        go : Tree -> List Tree -> List Tree -> Maybe Tree
        go ((Tree node _) as tree) rest acc =
            case rest of
                head :: tail ->
                    case evalStep head of
                        Just newHead ->
                            Just (Tree node (List.concat [ List.reverse acc, [ newHead ], tail ]))

                        Nothing ->
                            go tree tail (head :: acc)

                [] ->
                    evalRule tree
    in
    go root rootChildren []


evalSteps : Tree -> List Tree
evalSteps tree =
    Util.unfold evalStep (Just tree)


eval : Tree -> Maybe Tree
eval tree =
    case List.reverse (evalSteps tree) of
        result :: _ :: _ ->
            Just result

        _ ->
            Nothing

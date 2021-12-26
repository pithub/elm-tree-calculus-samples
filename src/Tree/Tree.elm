module Tree.Tree exposing (Node(..), Tree(..), add, bind, delta, eval, evalRule, evalStep, evalSteps, evaled, lambda, var)

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


evaled : Tree -> Tree
evaled tree =
    eval tree |> Maybe.withDefault tree



-- 4.2 Variable Binding


bind : String -> Tree -> List Tree -> Tree
bind label ((Tree tNode tChildren) as tree) children =
    case List.reverse tChildren of
        head :: tail ->
            delta (delta [ bind label head [] ] :: bind label (Tree tNode (List.reverse tail)) [] :: children)

        [] ->
            case tNode of
                Delta ->
                    delta (delta [] :: tree :: children)

                Var tLabel ->
                    if tLabel == label then
                        delta (delta [ delta [] ] :: delta [ delta [] ] :: children)

                    else
                        delta (delta [] :: tree :: children)


lambda : String -> Tree -> List Tree -> Tree
lambda label ((Tree tNode tChildren) as tree) children =
    case List.reverse tChildren of
        ((Tree hNode hChildren) as head) :: tail ->
            if
                (not (List.isEmpty hChildren) && contains label head)
                    || nodeContains label tNode
                    || List.any (contains label) tail
            then
                delta (delta [ lambda label head [] ] :: lambda label (Tree tNode (List.reverse tail)) [] :: children)

            else if nodeContains label hNode then
                Tree tNode (List.append (List.reverse tail) children)

            else
                delta (delta [] :: tree :: children)

        [] ->
            if nodeContains label tNode then
                delta (delta [ delta [] ] :: delta [ delta [] ] :: children)

            else
                delta (delta [] :: tree :: children)


contains : String -> Tree -> Bool
contains label (Tree node children) =
    nodeContains label node || List.any (contains label) children


nodeContains : String -> Node -> Bool
nodeContains label node =
    case node of
        Delta ->
            False

        Var vLabel ->
            vLabel == label

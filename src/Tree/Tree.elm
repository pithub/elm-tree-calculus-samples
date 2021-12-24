module Tree.Tree exposing (Node(..), Tree(..), add, delta, var)

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

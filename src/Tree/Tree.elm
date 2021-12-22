module Tree.Tree exposing (Node(..), Tree(..), delta, var)

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

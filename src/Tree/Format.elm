module Tree.Format exposing (toStrings)

import Tree.Tree as T


toStrings : T.Tree -> List String
toStrings (T.Tree kind children) =
    case kind of
        T.Delta ->
            addLevel "Δ" "" (toStringsN children)

        T.Var label ->
            addLevel label (String.repeat (String.length label + 1) " ") (toStringsN children)


addLevel : String -> String -> List String -> List String
addLevel text indent children =
    case List.map (String.append indent) children of
        head :: tail ->
            firstLine text head :: head :: tail

        [] ->
            [ text ]


firstLine : String -> String -> String
firstLine startOfFirstLine nextLine =
    String.toList nextLine
        |> List.drop (String.length startOfFirstLine)
        |> List.foldl
            (\char ( inWord, acc ) ->
                if isWordChar char then
                    if inWord then
                        ( True, "─" :: acc )

                    else
                        ( True, "┬" :: acc )

                else
                    ( False, "─" :: acc )
            )
            ( True, [] )
        |> Tuple.second
        |> adjustEndOfFirstLine ""
        |> String.append startOfFirstLine


adjustEndOfFirstLine : String -> List String -> String
adjustEndOfFirstLine acc rest =
    case rest of
        head :: tail ->
            if head == "─" then
                adjustEndOfFirstLine (String.append " " acc) tail

            else
                String.concat [ String.concat (List.reverse tail), "┐", acc ]

        [] ->
            acc


isWordChar : Char -> Bool
isWordChar char =
    let
        s : String
        s =
            String.fromChar char
    in
    not (s == " " || s == "─" || s == "┬" || s == "┐")


toStringsN : List T.Tree -> List String
toStringsN nodes =
    nodes
        |> List.map toStrings
        |> List.foldr
            (\list1 list2 ->
                case list1 of
                    head1 :: tail1 ->
                        case list2 of
                            head2 :: tail2 ->
                                combineLists
                                    (String.length head1 + String.length head2 + 1)
                                    tail1
                                    tail2
                                    [ String.concat [ head1, " ", head2 ] ]

                            [] ->
                                list1

                    [] ->
                        list2
            )
            []


combineLists : Int -> List String -> List String -> List String -> List String
combineLists len list1 list2 acc =
    case ( list1, list2 ) of
        ( head1 :: tail1, head2 :: tail2 ) ->
            combineLists len tail1 tail2 (String.concat [ head1, " ", head2 ] :: acc)

        ( _ :: _, [] ) ->
            List.append (List.reverse acc) (List.map (String.padRight len ' ') list1)

        ( [], _ :: _ ) ->
            List.append (List.reverse acc) (List.map (String.padLeft len ' ') list2)

        ( [], [] ) ->
            List.reverse acc

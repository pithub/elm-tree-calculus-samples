module Tree.Util exposing (unfold)


unfold : (a -> Maybe a) -> Maybe a -> List a
unfold fun maybe =
    let
        go : Maybe a -> List a -> List a
        go m acc =
            case m of
                Just val ->
                    go (fun val) (val :: acc)

                Nothing ->
                    List.reverse acc
    in
    go maybe []

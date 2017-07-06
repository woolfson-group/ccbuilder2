module Tools exposing (..)

chunks : Int -> List a -> List (List a)
chunks k xs =
    let
        len =
            List.length xs
    in
        if len > k then
            List.take k xs :: chunks k (List.drop k xs)
        else
            [ xs ]
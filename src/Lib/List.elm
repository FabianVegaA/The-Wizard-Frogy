module Lib.List exposing (at, index, rotate, splitAt)


rotate : Int -> List a -> List a
rotate n xs =
    case ( n, xs ) of
        ( 0, _ ) ->
            xs

        ( _, [] ) ->
            xs

        ( _, [ x ] ) ->
            [ x ]

        _ ->
            if n > 0 then
                let
                    ( first, rest ) =
                        splitAt (modBy (List.length xs) n) xs
                in
                rest ++ first

            else
                rotate (List.length xs + n) xs


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


index : a -> List a -> Int
index x xs =
    case xs of
        [] ->
            -1

        y :: ys ->
            if x == y then
                0

            else
                1 + index x ys


at : Int -> List a -> Maybe a
at n xs =
    case ( n, xs ) of
        ( _, [] ) ->
            Nothing

        ( 0, x :: _ ) ->
            Just x

        ( _, _ :: ys ) ->
            at (n - 1) ys

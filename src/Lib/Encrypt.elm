module Lib.Encrypt exposing (decrypt, encrypt, shiftChar)

import Lib.List exposing (at, index)


admissibleCodeChars : List Int
admissibleCodeChars =
    let
        nums =
            List.range 48 57

        lowChars =
            List.range 97 122

        hightChars =
            List.range 65 90

        simbolChars =
            List.map Char.toCode [ '-', '_', '.', '@', ',' ]
    in
    nums ++ hightChars ++ lowChars ++ simbolChars


shiftChar : Int -> Char -> Maybe Char
shiftChar shift char =
    let
        charCode =
            Char.toCode char
    in
    if List.member charCode admissibleCodeChars then
        index charCode admissibleCodeChars
            |> Maybe.map (\i -> modBy (List.length admissibleCodeChars) (i + shift))
            |> Maybe.andThen (\i -> at i admissibleCodeChars)
            |> Maybe.map Char.fromCode

    else
        Nothing


encrypt : Int -> String -> Maybe String
encrypt shift text =
    text
        |> String.toList
        |> List.map (\char -> shiftChar shift char)
        |> List.filterMap identity
        |> String.fromList
        |> Just


decrypt : Int -> String -> Maybe String
decrypt shift text =
    encrypt -shift text

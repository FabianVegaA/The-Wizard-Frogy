module Lib.Challenger exposing (Challenger, parseChallenger)


type alias Challenger =
    { name : String
    , score : Int
    }


parseChallenger : String -> Maybe Challenger
parseChallenger str =
    case String.split "," str of
        [ name, score ] ->
            String.toInt score
                |> Maybe.andThen (\s -> Just { name = name, score = s })

        _ ->
            Nothing

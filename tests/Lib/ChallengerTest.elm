module Lib.ChallengerTest exposing (..)

import Expect exposing (..)
import Lib.Challenger exposing (..)
import Lib.Encrypt exposing (..)
import Test exposing (..)


parseChallengerTest : Test
parseChallengerTest =
    describe "parse challenger"
        [ test "should return just" <|
            \_ ->
                let
                    name =
                        "foo"

                    score =
                        100

                    challenger =
                        name ++ "," ++ String.fromInt score
                in
                parseChallenger challenger
                    |> Expect.equal (Just { name = name, score = score })
        , test "should return nothing because of invalid score" <|
            \_ ->
                let
                    challenger =
                        "foo"
                in
                parseChallenger challenger
                    |> Expect.equal Nothing
        ]

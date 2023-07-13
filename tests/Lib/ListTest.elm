module Lib.ListTest exposing (splitAtTest, rotateTest)

import Expect exposing (..)
import Lib.List exposing (..)
import Test exposing (..)


splitAtTest : Test
splitAtTest =
    describe "split at index"
        [ test "split at 0" <|
            \_ ->
                splitAt 0 [ 1, 2, 3 ]
                    |> Expect.equal ( [], [ 1, 2, 3 ] )
        , test "split at an index" <|
            \_ ->
                splitAt 2 [ 1, 2, 3 ]
                    |> Expect.equal ( [ 1, 2 ], [ 3 ] )
        , test "split at the end" <|
            \_ ->
                splitAt 3 [ 1, 2, 3 ]
                    |> Expect.equal ( [ 1, 2, 3 ], [] )
        , test "split at an index larger than the list" <|
            \_ ->
                splitAt 4 [ 1, 2, 3 ]
                    |> Expect.equal ( [ 1, 2, 3 ], [] )
        , test "split at a negative index" <|
            \_ ->
                splitAt -1 [ 1, 2, 3 ]
                    |> Expect.equal ( [], [ 1, 2, 3 ] )
        ]


rotateTest : Test
rotateTest =
    describe "rotate a list"
        [ test "rotate 0" <|
            \_ ->
                rotate 0 [ 1, 2, 3 ]
                    |> Expect.equal [ 1, 2, 3 ]
        , test "rotate at an index" <|
            \_ ->
                rotate 2 [ 1, 2, 3 ]
                    |> Expect.equal [ 3, 1, 2 ]
        , test "rotate at the end" <|
            \_ ->
                rotate 3 [ 1, 2, 3 ]
                    |> Expect.equal [ 1, 2, 3 ]
        , test "rotate at an index larger than the list" <|
            \_ ->
                rotate 4 [ 1, 2, 3 ]
                    |> Expect.equal [ 2, 3, 1 ]
        , test "rotate at a negative index" <|
            \_ ->
                rotate -1 [ 1, 2, 3 ]
                    |> Expect.equal [ 3, 1, 2 ]
        ]

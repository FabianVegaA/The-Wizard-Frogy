module Lib.ListTest exposing (rotateTest, splitAtTest)

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


indexTest : Test
indexTest =
    describe "get index of any value in a list"
        [ test "index of 1" <|
            \_ ->
                index 1 [ 1, 2, 3 ]
                    |> Expect.equal (Just 0)
        , test "index of 2" <|
            \_ ->
                index 2 [ 1, 2, 3 ]
                    |> Expect.equal (Just 1)
        , test "index of 3" <|
            \_ ->
                index 3 [ 1, 2, 3 ]
                    |> Expect.equal (Just 2)
        , test "index of 4" <|
            \_ ->
                index 4 [ 1, 2, 3 ]
                    |> Expect.equal Nothing
        , test "index of 0" <|
            \_ ->
                index 0 [ 1, 2, 3 ]
                    |> Expect.equal Nothing
        , test "index of -1" <|
            \_ ->
                index -1 [ 1, 2, 3 ]
                    |> Expect.equal Nothing
        ]


atTest : Test
atTest =
    describe "get value at index"
        [ test "at 0" <|
            \_ ->
                at 0 [ 1, 2, 3 ]
                    |> Expect.equal (Just 1)
        , test "at an index" <|
            \_ ->
                at 2 [ 1, 2, 3 ]
                    |> Expect.equal (Just 3)
        , test "at the end" <|
            \_ ->
                at 3 [ 1, 2, 3 ]
                    |> Expect.equal Nothing
        , test "at an index larger than the list" <|
            \_ ->
                at 4 [ 1, 2, 3 ]
                    |> Expect.equal Nothing
        , test "at a negative index" <|
            \_ ->
                at -1 [ 1, 2, 3 ]
                    |> Expect.equal Nothing
        ]

module Lib.EncryptTest exposing (..)

import Expect exposing (..)
import Lib.Encrypt exposing (..)
import Test exposing (..)


shiftCharTest : Test
shiftCharTest =
    describe "shift a char"
        [ test "shifts a char with 0 shift" <|
            \_ ->
                shiftChar 0 'a'
                    |> Maybe.withDefault '?'
                    |> Expect.equal 'a'
        , test "shifts a char with 1 shift" <|
            \_ ->
                shiftChar 1 'a'
                    |> Maybe.withDefault '?'
                    |> Expect.equal 'b'
        ]


encryptTest : Test
encryptTest =
    describe "encrypt with shifting"
        [ test "encrypts a string with 0 shift" <|
            \_ ->
                encrypt 0 "abc"
                    |> Maybe.withDefault ""
                    |> Expect.equal "abc"
        , test "encrypts a string with 1 shift" <|
            \_ ->
                encrypt 1 "abc"
                    |> Maybe.withDefault ""
                    |> Expect.equal "bcd"
        , test "encrypts a string with 2 shift" <|
            \_ ->
                encrypt 2 "abc"
                    |> Maybe.withDefault ""
                    |> Expect.equal "cde"
        ]


decryptTest : Test
decryptTest =
    describe "decrypt with shifting"
        [ test "decrypts a string with 0 shift" <|
            \_ ->
                decrypt 0 "abc"
                    |> Maybe.withDefault ""
                    |> Expect.equal "abc"
        , test "decrypts a string with 1 shift" <|
            \_ ->
                decrypt 1 "bcd"
                    |> Maybe.withDefault ""
                    |> Expect.equal "abc"
        , test "decrypts a string with 2 shift" <|
            \_ ->
                decrypt 2 "cde"
                    |> Maybe.withDefault ""
                    |> Expect.equal "abc"
        ]

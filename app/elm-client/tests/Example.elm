module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Test exposing (..)


suite : Test
suite =
    describe "AbMarkdown Elm client"
        [ test "whatever" <|
            \_ -> Expect.equal (D.decodeString D.int "2") (Ok 2)
        , test "whatever again" <|
            \_ ->
                Expect.equal (Result.map (always ()) <| D.decodeString D.int "3")
                    (Ok ())
        ]



-- suite : Testsuite =    describe "AbMarkdown Elm client"
--         [test "Parse test nr 1" <|  \_ -> Expect.equal (Result.map (always ()) <|  D.decodeString decodeDoc "[]") (Ok ())
--         ]

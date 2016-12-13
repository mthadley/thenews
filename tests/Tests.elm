module Tests exposing (..)

import App
import Expect
import String
import Test exposing (..)
import DateFormat


all : Test
all =
    describe "TheNews Test Suite"
        [ describe "DateFormat"
            [ test "formats a unix timestamp as a string" <|
                \() ->
                    Expect.equal "Dec 12, 2016 at 18:40" <|
                        DateFormat.format 1481596806
            , test "pads certain times with zeroes" <|
                \() ->
                    Expect.equal "Dec 12, 2016 at 03:23" <|
                        DateFormat.format 1481541806
            ]
        ]

module DateFormatTests exposing (..)

import Expect
import Test exposing (..)
import DateFormat


all : Test
all =
    describe "TheNews Test Suite"
        [ describe "DateFormat"
            [ test "formats a unix timestamp as a string" <|
                \() ->
                    Expect.equal "Dec 12, 2016 at 11:23" <|
                        DateFormat.format 1481541806
            , test "pads certain times with zeroes" <|
                \() ->
                    Expect.equal "Dec 13, 2016 at 02:40" <|
                        DateFormat.format 1481596806
            ]
        ]

module Tests.Util.Html exposing (..)

import Expect
import Test exposing (..)
import Util.Html as Util


all : Test
all =
    describe "Util"
        [ describe "pluralize"
            [ test "should return the plural form " <|
                \() ->
                    Expect.equal "geese" <|
                        Util.pluralize "goose" "geese" 2
            , test "should return the singular form" <|
                \() ->
                    Expect.equal "goose" <|
                        Util.pluralize "goose" "geese" 1
            ]
        ]

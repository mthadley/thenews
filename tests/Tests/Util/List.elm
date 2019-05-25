module Tests.Util.List exposing (tests)

import Expect
import Test exposing (..)
import Util.List as Util


tests : Test
tests =
    describe "Util.List"
        [ describe "takeMap"
            [ test "it should return empty list" <|
                \() ->
                    Util.takeMap (\a -> Just a) []
                        |> Expect.equal []
            , test "it should return the single Just" <|
                \() ->
                    Util.takeMap (\a -> Just a) [ 1 ]
                        |> Expect.equal [ 1 ]
            , test "it should not return the single Nothing" <|
                \() ->
                    Util.takeMap (\a -> Nothing) [ 1 ]
                        |> Expect.equal []
            , test "it should return only up the first Nothing" <|
                \() ->
                    Util.takeMap
                        (\a ->
                            if a < 3 || a > 5 then
                                Just a

                            else
                                Nothing
                        )
                        [ 1, 2, 3, 4, 5, 6 ]
                        |> Expect.equal [ 1, 2 ]
            ]
        ]

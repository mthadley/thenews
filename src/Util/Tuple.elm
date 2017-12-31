module Util.Tuple exposing (mapSecond, mapThird)


mapThird : (c -> d) -> ( a, b, c ) -> ( a, b, d )
mapThird f ( a, b, c ) =
    ( a, b, f c )


mapSecond : (b -> d) -> ( a, b, c ) -> ( a, d, c )
mapSecond f ( a, b, c ) =
    ( a, f b, c )

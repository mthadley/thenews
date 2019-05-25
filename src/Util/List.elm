module Util.List exposing (takeMaybe)

{-|

@docs takeMaybe

-}


{-| Like `takeWhile` combined with `filterMap`.
TODO: Write a test for this
-}
takeMaybe : (a -> Maybe b) -> List a -> List b
takeMaybe f list =
    List.head list
        |> Maybe.andThen f
        |> Maybe.map2
            (\list_ x -> x :: takeMaybe f list_)
            (List.tail list)
        |> Maybe.withDefault []

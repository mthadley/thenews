module Util.List exposing (takeMaybe, zip)


takeMaybe : (a -> Maybe b) -> List a -> List b
takeMaybe f list =
    List.head list
        |> Maybe.andThen f
        |> Maybe.map2
            (\list x -> x :: takeMaybe f list)
            (List.tail list)
        |> Maybe.withDefault []


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys

        ( _, _ ) ->
            []

module Util exposing (..)

import Elements
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode


empty : Html msg
empty =
    text ""


jsLink : Attribute msg
jsLink =
    Attr.href "javascript:;"


maybeToString : Maybe a -> Maybe String
maybeToString =
    Maybe.map toString


mapThird : (c -> d) -> ( a, b, c ) -> ( a, b, d )
mapThird f ( a, b, c ) =
    ( a, b, f c )


mapSecond : (b -> d) -> ( a, b, c ) -> ( a, d, c )
mapSecond f ( a, b, c ) =
    ( a, f b, c )


optionalMaybe : String -> Decoder a -> (Decoder (Maybe a -> b) -> Decoder b)
optionalMaybe field decoder =
    optional field (Decode.map Just decoder) Nothing


pluralize : String -> String -> Int -> String
pluralize singular plural count =
    if count > 1 then
        plural
    else
        singular


takeMaybe : (a -> Maybe b) -> List a -> List b
takeMaybe f list =
    List.head list
        |> Maybe.andThen f
        |> Maybe.map2
            (\list x -> x :: takeMaybe f list)
            (List.tail list)
        |> Maybe.withDefault []


viewHtmlContent : String -> Html msg
viewHtmlContent content =
    Elements.htmlContent
        [ Attr.property "innerHTML" <| Encode.string content ]
        []


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        empty


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe f =
    Maybe.withDefault empty << Maybe.map f


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys

        ( _, _ ) ->
            []

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


optionalMaybe : String -> Decoder a -> (Decoder (Maybe a -> b) -> Decoder b)
optionalMaybe field decoder =
    optional field (Decode.map Just decoder) Nothing


pluralize : String -> String -> Int -> String
pluralize singular plural count =
    if count > 1 then
        plural
    else
        singular


viewHtmlContent : String -> Html msg
viewHtmlContent content =
    Elements.htmlContent
        [ Attr.property "innerHTML" <| Encode.string content
        ]
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

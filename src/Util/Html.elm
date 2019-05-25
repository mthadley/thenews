module Util.Html exposing
    ( empty
    , pluralize
    , viewHtmlContent
    , viewIf
    , viewMaybe
    )

import Elements
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Json.Encode as Encode


empty : Html msg
empty =
    text ""


pluralize : String -> String -> Int -> String
pluralize singular plural count =
    if count > 1 then
        plural

    else
        singular


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

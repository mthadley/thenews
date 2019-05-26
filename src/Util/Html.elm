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
        [ Attr.property "content" <| Encode.string content ]
        []


viewIf : (() -> Html msg) -> Bool -> Html msg
viewIf f condition =
    if condition then
        f ()

    else
        empty


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe f =
    Maybe.withDefault empty << Maybe.map f

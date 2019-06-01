port module Util.Html exposing
    ( empty
    , pluralize
    , postContentLinkClicks
    , viewHtmlContent
    , viewIf
    , viewMaybe
    )

import Css exposing (px)
import Css.Global as Global
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
    styled (node "post-content")
        [ Css.marginBottom <| px 12
        , Css.display Css.block
        , Global.descendants
            [ Global.pre
                [ Css.overflow Css.hidden
                , Css.textOverflow Css.ellipsis
                ]
            ]
        ]
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


port postContentLinkClicks : (String -> msg) -> Sub msg

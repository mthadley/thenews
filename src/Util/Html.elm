module Util.Html exposing
    ( empty
    , pluralize
    , viewHtmlContent
    , viewIf
    , viewMaybe
    )

import Css exposing (px)
import Css.Global as Global
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Json.Decode as Decode
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


viewHtmlContent : (String -> msg) -> String -> Html msg
viewHtmlContent toLinkClickMsg content =
    let
        clickDecoder =
            Decode.at [ "target", "href" ] Decode.string
                |> Decode.map (\href -> ( toLinkClickMsg href, True ))
    in
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
        [ Attr.property "content" <| Encode.string content
        , Events.preventDefaultOn "click" clickDecoder
        ]
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

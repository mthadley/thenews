module Theme exposing
    ( Theme(..)
    , colors
    , commentLevelMargin
    , fontSizes
    , fromString
    , termShadow
    , termShadowText
    )

import Css exposing (..)
import Json.Decode as Decode exposing (Decoder)


type Theme
    = Dark
    | Light


fromString : String -> Maybe Theme
fromString string =
    case string of
        "light" ->
            Just Light

        "dark" ->
            Just Dark

        _ ->
            Nothing


colors : Theme -> { primary : Color, secondary : Color }
colors theme =
    case theme of
        Dark ->
            { primary = hex "F49E42"
            , secondary = hex "111"
            }

        Light ->
            { primary = hex "111"
            , secondary = hex "F49E42"
            }


fontSizes : { comment : Px, base : Px, h2 : Px, h3 : Px }
fontSizes =
    { comment = px 14, base = px 16, h2 = px 18, h3 = px 16 }


commentLevelMargin : Float
commentLevelMargin =
    8


termShadow : Theme -> Style
termShadow theme =
    boxShadow4 (px 0) (px 0) (px 4) (colors theme).primary


termShadowText : Theme -> Style
termShadowText theme =
    textShadow4 (px 0) (px 0) (px 4) (colors theme).primary

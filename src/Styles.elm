module Styles exposing (styles)

import Css exposing (..)
import Css.Global exposing (..)
import Html.Styled exposing (Html)
import Theme exposing (Theme, fontSizes, termShadowText)


styles : Theme -> Html msg
styles theme =
    let
        { primary, secondary } =
            Theme.colors theme
    in
    global
        [ everything
            [ boxSizing borderBox
            ]
        , a
            [ color primary
            , textDecoration none
            , property "transition" "opacity 0.4s ease"
            , hover
                [ opacity <| num 0.7
                ]
            ]
        , body
            [ backgroundColor secondary
            , color primary
            , fontFamily monospace
            , fontSize fontSizes.base
            , margin zero
            , minHeight <| vh 100
            , position relative
            , termShadowText theme
            , property "-webkit-tap-highlight-color" primary.value
            , before
                [ backgroundImage <|
                    linearGradient
                        -- lighten secondary 50%
                        (stop2 (hex "919191") <| pct 50)
                        (stop2 (rgba 0 0 0 0) <| pct 50)
                        []
                , backgroundSize2 (pct 100) <| px 4
                , backgroundRepeat repeatY
                , property "content" "''"
                , height <| pct 100
                , opacity <| num 0.1
                , property "pointer-events" "none"
                , position absolute
                , width <| pct 100
                ]
            ]
        , h2
            [ fontSize fontSizes.h2
            ]
        , h3
            [ fontSize fontSizes.h3
            ]
        , html
            -- Lighten secondary 5%
            [ backgroundColor <| hex "1E1E1E"
            ]
        , header
            [ display inlineBlock
            , marginBottom <| px 16
            ]
        , p
            [ margin2 (px 12) zero
            ]
        ]

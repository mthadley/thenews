module Elements exposing (Element, author, categoryItem, comment, commentLevel, container, header, headerAscii, headerTagline, htmlContent, item, itemHeader, itemRank, nav, navLink, navList, showMore)

import Css exposing (..)
import Css.Media as Media exposing (only, screen, withMedia)
import Html.Styled as Html exposing (..)
import Theme
    exposing
        ( colors
        , commentLevelMargin
        , fontSizes
        , termShadow
        )


{-| Type alias for a standard Html element function.
-}
type alias Element msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


author : Element msg
author =
    styled Html.h3 [ fontStyle italic ]


comment : Element msg
comment =
    styled Html.article
        [ fontSize fontSizes.comment
        , marginBottom <| px 40
        ]


commentLevel : Element msg
commentLevel =
    styled Html.div
        [ marginLeft (px commentLevelMargin)
        , position relative
        , after
            [ backgroundColor colors.primary
            , termShadow
            , property "content" "''"
            , height <| pct 100
            , left <| px (-1 * commentLevelMargin)
            , opacity <| num 0.8
            , position absolute
            , top zero
            , width <| px 2
            ]
        ]


header : Element msg
header =
    styled Html.header
        [ display inlineBlock
        , marginBottom <| px 16
        ]


headerAscii : Element msg
headerAscii =
    styled Html.h1
        [ fontSize <| vw 2.5
        , maxWidth <| pct 100
        , whiteSpace Css.pre
        , withMedia [ only screen [ Media.minWidth <| px 768 ] ]
            [ fontSize <| px 16
            ]
        ]


headerTagline : Element msg
headerTagline =
    styled Html.p [ textAlign end ]


item : Element msg
item =
    styled Html.article [ marginBottom <| px 16 ]


itemHeader : Element msg
itemHeader =
    styled Html.h2 [ margin3 zero zero <| px 12 ]


nav : Element msg
nav =
    styled Html.nav
        [ backgroundColor colors.primary
        , termShadow
        , whiteSpace Css.pre
        ]


navList : Element msg
navList =
    styled Html.ul
        [ displayFlex
        , flexWrap wrap
        , listStyle none
        , paddingLeft <| px 48
        ]


navLink : Element msg
navLink =
    styled Html.a
        [ color colors.secondary
        , marginRight <| px 8
        ]


categoryItem : Element msg
categoryItem =
    styled Html.div [ displayFlex ]


itemRank : Element msg
itemRank =
    styled Html.div
        [ flexShrink zero
        , fontSize <| px 18
        , width <| px 48
        ]


container : Element msg
container =
    styled Html.div
        [ margin2 zero auto
        , maxWidth <| px 768
        , padding <| px 8
        ]


htmlContent : Element msg
htmlContent =
    styled Html.div [ marginBottom <| px 12 ]


showMore : Element msg
showMore =
    styled Html.a [ fontWeight bold ]

module Theme exposing (..)

import Css exposing (..)


colors : { primary : Color, secondary : Color }
colors =
    { primary = hex "F49E42"
    , secondary = hex "111"
    }


fontSizes : { comment : Px, base : Px, h2 : Px, h3 : Px }
fontSizes =
    { comment = px 14, base = px 16, h2 = px 18, h3 = px 16 }


commentLevelMargin : Px
commentLevelMargin =
    px 8


termShadow : Style
termShadow =
    boxShadow4 (px 0) (px 0) (px 4) colors.primary


termShadowText : Style
termShadowText =
    textShadow4 (px 0) (px 0) (px 4) colors.primary

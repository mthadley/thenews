module Views.Header exposing (view)

import Css exposing (pct, px, vw)
import Css.Media as Media exposing (only, screen, withMedia)
import Data.Category as Category
import Html.Styled exposing (Html, a, h1, header, p, styled, text)
import Html.Styled.Attributes as Attr
import Router


splash : String
splash =
    """
 ________       _  __
/_  __/ /  ___ / |/ /__ _    _____
 / / / _ \\/ -_)    / -_) |/|/ (_-<
/_/ /_//_/\\__/_/|_/\\__/|__,__/___/
"""


view : Html msg
view =
    styled header
        [ Css.display Css.inlineBlock
        , Css.marginBottom <| Css.px 16
        ]
        []
        [ styled h1
            [ Css.fontSize <| vw 2.5
            , Css.maxWidth <| pct 100
            , Css.whiteSpace Css.pre
            , withMedia [ only screen [ Media.minWidth <| px 768 ] ]
                [ Css.fontSize <| px 16
                ]
            ]
            []
            [ a [ Router.linkTo <| Router.View Category.Top ] [ text splash ]
            ]
        , styled p
            [ Css.textAlign Css.end ]
            []
            [ text "Thanks, "
            , a [ Attr.href "https://news.ycombinator.com" ] [ text "YC!" ]
            ]
        ]

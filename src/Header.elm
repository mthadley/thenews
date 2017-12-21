module Header exposing (view)

import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Router
import Api


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
    header []
        [ h1 [ Attr.class "header-ascii" ]
            [ a [ Router.linkTo <| Router.View Api.Top ] [ text splash ]
            ]
        , p [ Attr.class "header-tagline" ]
            [ text "Thanks, "
            , a [ Attr.href "https://news.ycombinator.com" ] [ text "YC!" ]
            ]
        ]

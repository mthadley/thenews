module Views.Header exposing (view)

import Api
import Elements
import Html.Styled exposing (..)
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
    Elements.header []
        [ Elements.headerAscii []
            [ a [ Router.linkTo <| Router.View Api.Top ] [ text splash ]
            ]
        , Elements.headerTagline []
            [ text "Thanks, "
            , a [ Attr.href "https://news.ycombinator.com" ] [ text "YC!" ]
            ]
        ]

module Views.Nav exposing (view)

import Data.Category as Category exposing (Category)
import Elements
import Html.Styled exposing (..)
import Router exposing (Route)
import Theme exposing (Theme)


isActive : Route -> Category -> Bool
isActive route category =
    case route of
        Router.View current ->
            current == category

        _ ->
            False


navLinks : List ( String, Category )
navLinks =
    List.map
        (\category -> ( Category.toString category, category ))
        [ Category.Top
        , Category.Best
        , Category.Show
        , Category.New
        , Category.Ask
        , Category.Job
        ]


view : Theme -> Route -> Html msg
view theme route =
    Elements.nav theme
        []
        [ Elements.navList [] <| List.map (viewNavItem theme route) navLinks
        ]


viewNavItem : Theme -> Route -> ( String, Category ) -> Html msg
viewNavItem theme route ( name, category ) =
    let
        indicator =
            if isActive route category then
                "*"

            else
                " "
    in
    li []
        [ Elements.navLink theme
            [ Router.linkTo <| Router.View category ]
            [ text <| indicator ++ name ]
        ]

module Views.Nav exposing (view)

import Data.Category as Category exposing (Category)
import Elements
import Html.Styled exposing (..)
import Router exposing (Route)


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


view : Route -> Html msg
view route =
    Elements.nav []
        [ Elements.navList [] <| List.map (viewNavItem route) navLinks
        ]


viewNavItem : Route -> ( String, Category ) -> Html msg
viewNavItem route ( name, category ) =
    let
        indicator =
            if isActive route category then
                "*"

            else
                " "
    in
    li []
        [ Elements.navLink [ Router.linkTo <| Router.View category ]
            [ text <| indicator ++ name ]
        ]

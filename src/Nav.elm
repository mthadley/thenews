module Nav exposing (view)

import Html exposing (..)
import Router exposing (Route)
import Api exposing (Category)


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
        (\category -> ( Api.label category, category ))
        [ Api.Top
        , Api.Best
        , Api.Show
        , Api.New
        , Api.Ask
        , Api.Job
        ]


view : Route -> Html msg
view route =
    nav []
        [ ul [] <| List.map (viewNavItem route) navLinks
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
            [ a [ Router.linkTo <| Router.View category ]
                [ text <| indicator ++ name ]
            ]

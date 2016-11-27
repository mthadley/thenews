module Nav exposing (view)

import Html exposing (..)
import Html.Attributes as Attr
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
    [ ( "Top", Api.Top )
    , ( "Best", Api.Best )
    , ( "Show", Api.Show )
    , ( "New", Api.New )
    , ( "Ask", Api.Ask )
    , ( "Job", Api.Job )
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
            [ a [ Attr.href <| Router.reverse <| Router.View category ]
                [ text <| indicator ++ name ]
            ]

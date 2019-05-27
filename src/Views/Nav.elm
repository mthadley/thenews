module Views.Nav exposing (view)

import Css exposing (px)
import Data.Category as Category exposing (Category)
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
    styled nav
        [ Css.backgroundColor (Theme.colors theme).primary
        , Theme.termShadow theme
        , Css.whiteSpace Css.pre
        ]
        []
        [ styled ul
            [ Css.displayFlex
            , Css.flexWrap Css.wrap
            , Css.listStyle Css.none
            , Css.paddingLeft <| px 48
            ]
            []
            (List.map (viewNavItem theme route) navLinks)
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
        [ styled a
            [ Css.color (Theme.colors theme).secondary
            , Css.marginRight <| px 8
            ]
            [ Router.linkTo <| Router.View category ]
            [ text <| indicator ++ name ]
        ]

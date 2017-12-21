module Main exposing (..)

import App exposing (Model, Msg, init, subscriptions, update, view)
import Router
import Navigation
import Html.Styled exposing (toUnstyled)


main : Program Never Model Msg
main =
    Navigation.program
        (App.RouteChange << Router.parseLocation)
        { init = init << Router.parseLocation
        , view = toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }

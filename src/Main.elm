module Main exposing (..)

import App exposing (Model, Msg, init, subscriptions, update, view)
import Html.Styled exposing (toUnstyled)
import Navigation
import Router


main : Program Never Model Msg
main =
    Navigation.program
        (App.RouteChange << Router.parseLocation)
        { init = init << Router.parseLocation
        , view = toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }

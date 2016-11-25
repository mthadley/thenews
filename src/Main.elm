module Main exposing (..)

import App exposing (Model, Msg, init, subscriptions, update, view)
import Router
import Navigation


main : Program Never Model Msg
main =
    Navigation.program
        (App.RouteChange << Router.parseLocation)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

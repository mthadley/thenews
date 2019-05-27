module Main exposing (main)

import App exposing (Flags, Model, Msg, init, subscriptions, update, view)
import Browser.Hash
import Html.Styled exposing (toUnstyled)
import Router


main : Program Flags Model Msg
main =
    Browser.Hash.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = App.UrlRequest
        , onUrlChange = App.UrlChange << Router.parse
        }

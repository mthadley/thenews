module Router exposing (..)

import Api exposing (Category(..))
import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = View Category
    | NotFound


parseRoute : Parser (Route -> a) a
parseRoute =
    oneOf
        [ map (View Top) top
        , map (View Ask) <| s "ask"
        , map (View Best) <| s "best"
        , map (View Job) <| s "job"
        , map (View New) <| s "new"
        , map (View Show) <| s "show"
        , map (View Top) <| s "top"
        ]


parseLocation : Location -> Route
parseLocation =
    Maybe.withDefault NotFound << parseHash parseRoute


reverse : Route -> String
reverse route =
    let
        path =
            case route of
                View Ask ->
                    "ask"

                View Best ->
                    "best"

                View Job ->
                    "job"

                View New ->
                    "new"

                View Show ->
                    "show"

                View Top ->
                    "top"

                NotFound ->
                    "404"
    in
        "#" ++ path

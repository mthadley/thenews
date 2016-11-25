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
        [ map (View Ask) <| s "ask"
        , map (View Best) <| s "best"
        , map (View Job) <| s "job"
        , map (View New) <| s "new"
        , map (View Show) <| s "show"
        , map (View Top) <| s "top"
        ]


parseLocation : Location -> Route
parseLocation =
    Maybe.withDefault NotFound << parseHash parseRoute

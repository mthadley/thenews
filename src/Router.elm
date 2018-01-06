module Router exposing (..)

import Api exposing (Category(..))
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Navigation exposing (Location)
import Tagged
import Types.Item as Item
import Types.User as User
import UrlParser exposing (..)


type Route
    = View Category
    | ViewItem Item.Id
    | ViewUser User.Id
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
        , map (ViewItem << Tagged.tag) <| s "item" </> int
        , map (ViewUser << Tagged.tag) <| s "user" </> string
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

                ViewItem id ->
                    "item/" ++ (toString <| Tagged.untag id)

                ViewUser id ->
                    "user/" ++ Tagged.untag id
    in
    "#" ++ path


linkTo : Route -> Html.Attribute msg
linkTo =
    Attr.href << reverse

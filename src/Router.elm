module Router exposing (Route(..), linkTo, parse, reverse)

import Data.Category exposing (Category(..))
import Data.Item as Item
import Data.User as User
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Tagged
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), Parser)


type Route
    = View Category
    | ViewItem Item.Id
    | ViewUser User.Id
    | NotFound


parser : Parser (Route -> a) a
parser =
    Url.oneOf
        [ Url.map (View Top) Url.top
        , Url.map (View Ask) <| Url.s "ask"
        , Url.map (View Best) <| Url.s "best"
        , Url.map (View Job) <| Url.s "job"
        , Url.map (View New) <| Url.s "new"
        , Url.map (View Show) <| Url.s "show"
        , Url.map (View Top) <| Url.s "top"
        , Url.map (ViewItem << Tagged.tag) <| Url.s "item" </> Url.int
        , Url.map (ViewUser << Tagged.tag) <| Url.s "user" </> Url.string
        ]


parse : Url -> Route
parse =
    Maybe.withDefault NotFound << Url.parse parser


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
                    "item/" ++ (String.fromInt <| Tagged.untag id)

                ViewUser id ->
                    "user/" ++ Tagged.untag id
    in
    "#" ++ path


linkTo : Route -> Html.Attribute msg
linkTo =
    Attr.href << reverse

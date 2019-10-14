module Router exposing (Route(..), linkTo, parse, parseExternal, redirectExternal, reverse)

import Browser.Navigation as Navigation
import Data.Category exposing (Category(..))
import Data.Item as Item
import Data.User as User
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Maybe.Extra
import Tagged
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


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


parseExternal : String -> Maybe Route
parseExternal href =
    let
        withRequired route p =
            Url.map (Maybe.map (route << Tagged.tag)) p

        with route p =
            Url.map (Just route) p

        externalParser =
            Url.oneOf
                [ withRequired ViewItem <| Url.s "item" <?> Query.int "id"
                , withRequired ViewUser <| Url.s "user" <?> Query.string "id"
                , with (View Ask) <| Url.s "ask"
                , with (View Show) <| Url.s "show"
                , with (View Job) <| Url.s "jobs"
                , with (View New) <| Url.s "newest"
                ]

        parseExternalRoute url =
            if url.host == "news.ycombinator.com" then
                Maybe.Extra.join <| Url.parse externalParser url

            else
                Nothing
    in
    Url.fromString href
        |> Maybe.andThen parseExternalRoute


redirectExternal : Navigation.Key -> String -> Cmd msg
redirectExternal key href =
    case parseExternal href of
        Just internalRoute ->
            Navigation.pushUrl key (reverse internalRoute)

        Nothing ->
            Navigation.load href


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

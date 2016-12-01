module UserPage exposing (Model, Msg(RouteChange), init, update, view)

import Api
import Html exposing (..)
import RemoteData exposing (RemoteData(..))
import Router exposing (Route)
import User exposing (User)
import Util


-- MODEL


type alias Model =
    { user : RemoteData User
    }


init : Route -> ( Model, Cmd Msg )
init route =
    updateRoute (Model NotRequested) route



-- VIEW


view : Model -> Html Msg
view model =
    case model.user of
        Done user ->
            section []
                [ viewUser user
                ]

        Loading ->
            text "Loading..."

        _ ->
            text "There doesn't seem to be anything here."


viewUser : User -> Html Msg
viewUser user =
    div []
        [ h2 [] [ text user.id ]
        , Util.viewHtmlContent user.about
        ]



-- UPDATE


type Msg
    = RouteChange Route
    | ReceiveUser (Api.Result User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteChange route ->
            updateRoute model route

        ReceiveUser result ->
            ( { model | user = RemoteData.fromResult result }, Cmd.none )


updateRoute : Model -> Route -> ( Model, Cmd Msg )
updateRoute model route =
    case route of
        Router.ViewUser id ->
            if RemoteData.isDone model.user && id == getId model.user then
                model ! []
            else
                ( { model | user = Loading }
                , fetchUser id
                )

        _ ->
            model ! []


fetchUser : String -> Cmd Msg
fetchUser =
    Api.send ReceiveUser << Api.requestUser


getId : RemoteData User -> String
getId =
    RemoteData.withDefault "" << RemoteData.map .id

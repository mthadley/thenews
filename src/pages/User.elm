module Pages.User exposing (Model, Msg(RouteChange), init, update, view)

import Api
import Html exposing (..)
import Item exposing (Item)
import ItemEntry
import RemoteData exposing (RemoteData(..))
import Router exposing (Route)
import User exposing (User)
import Util


-- MODEL


type alias Model =
    { user : RemoteData User
    , items : RemoteData (List Item)
    }


init : Route -> ( Model, Cmd Msg )
init route =
    updateRoute (Model NotRequested NotRequested) route



-- VIEW


view : Model -> Html Msg
view model =
    case model.user of
        Done user ->
            section []
                [ viewUser user
                , viewSubmissions model.items
                ]

        Loading ->
            text "Loading..."

        _ ->
            text "There doesn't seem to be anything here."


viewUser : User -> Html Msg
viewUser user =
    div []
        [ h3 [] [ text user.id ]
        , user.about
            |> Maybe.map Util.viewHtmlContent
            |> Maybe.withDefault Util.empty
        ]


viewSubmissions : RemoteData (List Item) -> Html Msg
viewSubmissions items =
    let
        details =
            [ ItemEntry.Score, ItemEntry.Comments, ItemEntry.Created ]

        content =
            case items of
                Done items ->
                    List.map (ItemEntry.view True details) items

                Loading ->
                    [ text "Loading..." ]

                _ ->
                    [ text "There doesn't seem to be anything here." ]
    in
        section [] <|
            [ h2 [] [ text "▬ Recent Submissions" ]
            , div [] content
            ]



-- UPDATE


type Msg
    = RouteChange Route
    | ReceiveUser (Api.Result User)
    | ReceiveItems (Api.Result (List Item))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteChange route ->
            updateRoute model route

        ReceiveUser user ->
            ( { model
                | user = RemoteData.fromResult user
                , items = Loading
              }
            , user
                |> Result.map .submitted
                |> Result.withDefault []
                |> List.take 10
                |> fetchItems
            )

        ReceiveItems items ->
            { model | items = RemoteData.fromResult items } ! []


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


fetchItems : List Int -> Cmd Msg
fetchItems =
    Api.send ReceiveItems << Api.requestItems


getId : RemoteData User -> String
getId =
    RemoteData.withDefault "" << RemoteData.map .id

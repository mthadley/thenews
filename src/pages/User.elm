module Pages.User exposing (Model, Msg(RouteChange), init, subscriptions, update, view)

import Api
import Html exposing (..)
import Item exposing (Item)
import ItemEntry
import RemoteData exposing (RemoteData(..))
import Router exposing (Route)
import User exposing (User)
import Util
import LoadText


-- MODEL


type alias Model =
    { user : RemoteData User
    , items : RemoteData (List Item)
    , loadText : LoadText.Model
    }


init : Route -> ( Model, Cmd Msg )
init route =
    updateRoute
        (Model NotRequested NotRequested (LoadText.init False))
        route



-- VIEW


view : Model -> Html Msg
view model =
    case model.user of
        Done user ->
            section []
                [ viewUser user
                , viewSubmissions model
                ]

        Loading ->
            LoadText.view model.loadText

        _ ->
            text "There doesn't seem to be anything here."


viewUser : User -> Html Msg
viewUser user =
    div []
        [ h3 [] [ text user.id ]
        , Util.viewMaybe Util.viewHtmlContent user.about
        ]


viewSubmissions : Model -> Html Msg
viewSubmissions { items, loadText } =
    let
        details =
            [ ItemEntry.Score, ItemEntry.Comments, ItemEntry.Created ]

        content =
            case items of
                Done items ->
                    List.map (ItemEntry.view True details) items

                Loading ->
                    [ LoadText.view loadText ]

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
    | LoadTextMsg LoadText.Msg


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
            { model
                | items = RemoteData.fromResult items
                , loadText = LoadText.toggle False model.loadText
            }
                ! []

        LoadTextMsg childMsg ->
            { model | loadText = LoadText.update childMsg model.loadText } ! []


updateRoute : Model -> Route -> ( Model, Cmd Msg )
updateRoute model route =
    case route of
        Router.ViewUser id ->
            if RemoteData.isDone model.user && id == getId model.user then
                model ! []
            else
                ( { model
                    | user = Loading
                    , loadText = LoadText.toggle True model.loadText
                  }
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map LoadTextMsg <| LoadText.subscriptions model.loadText

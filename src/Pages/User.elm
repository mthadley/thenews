module Pages.User exposing (Model, Msg, init, subscriptions, update, view)

import Data.Item exposing (Item)
import Data.User as User exposing (User)
import Html.Styled exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Store exposing (Action, Store)
import Tagged
import Time
import Util.Html exposing (viewHtmlContent, viewMaybe)
import Views.Item as ItemView
import Views.LoadText as LoadText



-- MODEL


type alias Model =
    { user : User.Id
    , loadText : LoadText.Model
    }


init : User.Id -> ( Model, Action Msg )
init user =
    ( Model user LoadText.init
    , Store.tag RecieveUser <| Store.requestUser user
    )



-- VIEW


view : Store -> Model -> ( String, Html msg )
view store model =
    case Store.getUser store model.user of
        Success user ->
            ( Tagged.untag user.id
            , section []
                [ viewUser user
                , viewSubmissions (Store.getZone store) model <| getUserItems store user.id
                ]
            )

        Loading ->
            ( LoadText.viewString model.loadText, LoadText.view model.loadText )

        _ ->
            ( "Nothing...", text "There doesn't seem to be anything here." )


viewUser : User -> Html msg
viewUser user =
    div []
        [ h3 [] [ text <| Tagged.untag user.id ]
        , viewMaybe viewHtmlContent user.about
        ]


viewSubmissions : Time.Zone -> Model -> WebData (List Item) -> Html msg
viewSubmissions zone { loadText } items =
    let
        content =
            case items of
                Success items_ ->
                    List.map
                        (ItemView.view zone
                            [ ItemView.textContent
                            , ItemView.score
                            , ItemView.comments
                            , ItemView.created
                            ]
                        )
                        items_

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
    = LoadTextMsg LoadText.Msg
    | RecieveUser


update : Store -> Msg -> Model -> ( Model, Action Msg )
update store msg model =
    case msg of
        LoadTextMsg childMsg ->
            ( { model | loadText = LoadText.update childMsg model.loadText }, Store.none )

        RecieveUser ->
            Store.getUser store model.user
                |> RemoteData.map .submitted
                |> RemoteData.withDefault []
                |> List.take Store.pageSize
                |> List.map Store.requestItem
                |> Store.batch
                |> Tuple.pair model


getUserItems : Store -> User.Id -> WebData (List Item)
getUserItems store =
    Store.getUser store
        >> RemoteData.map (List.take Store.pageSize << .submitted)
        >> RemoteData.andThen (Store.getItems store)



-- SUBS


subscriptions : Store -> Model -> Sub Msg
subscriptions store model =
    Store.getUser store model.user
        |> RemoteData.andThen (getUserItems store << .id)
        |> RemoteData.isLoading
        |> LoadText.subscriptions
        |> Sub.map LoadTextMsg

module Pages.User exposing (Model, Msg, init, subscriptions, update, view)

import Html.Styled exposing (..)
import PageTitle
import RemoteData exposing (RemoteData(..), WebData)
import Store exposing (Action, Store)
import Types.Item exposing (Item)
import Types.User as User exposing (User)
import Util.Html exposing (viewMaybe, viewHtmlContent)
import Views.Item as ItemView
import Views.LoadText as LoadText


-- MODEL


type alias Model =
    { user : String
    , loadText : LoadText.Model
    }


init : String -> ( Model, Cmd msg, Action Msg )
init user =
    ( Model user LoadText.init
    , PageTitle.set user
    , Store.tag RecieveUser <| Store.requestUser user
    )



-- VIEW


view : Store -> Model -> Html msg
view store model =
    case Store.getUser store model.user of
        Success user ->
            section []
                [ viewUser user
                , viewSubmissions model <| getUserItems store user.id
                ]

        Loading ->
            LoadText.view model.loadText

        _ ->
            text "There doesn't seem to be anything here."


viewUser : User -> Html msg
viewUser user =
    div []
        [ h3 [] [ text user.id ]
        , viewMaybe viewHtmlContent user.about
        ]


viewSubmissions : Model -> WebData (List Item) -> Html msg
viewSubmissions { loadText } items =
    let
        details =
            [ ItemView.Score, ItemView.Comments, ItemView.Created ]

        content =
            case items of
                Success items ->
                    List.map (ItemView.view True details) items

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
                |> (,) model


getUserItems : Store -> String -> WebData (List Item)
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

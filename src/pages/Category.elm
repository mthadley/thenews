module Pages.Category exposing (..)

import Api exposing (Category)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Item exposing (Item)
import ItemEntry
import LoadText
import RemoteData exposing (RemoteData(..))
import Router exposing (Route)


-- MODEL


type alias Model =
    { category : Category
    , items : ItemsCache
    , loadText : LoadText.Model
    }


type alias ItemsCache =
    Dict String RemoteItems


type alias RemoteItems =
    RemoteData (List Item)


init : Route -> ( Model, Cmd Msg )
init route =
    updateRoute (Model Api.Best Dict.empty (LoadText.init False)) route



-- VIEW


view : Model -> Html msg
view model =
    case getData model.category model.items of
        Done items ->
            section [] <| List.indexedMap viewCategoryItem items

        Loading ->
            LoadText.view model.loadText

        _ ->
            text "There doesn't seem to be anything here."


viewCategoryItem : Int -> Item -> Html msg
viewCategoryItem rank item =
    div [ Attr.class "category-item" ]
        [ div [ Attr.class "item-rank" ] [ text <| "#" ++ (toString <| 1 + rank) ]
        , ItemEntry.view False [ ItemEntry.By, ItemEntry.Score, ItemEntry.Comments ] item
        ]



-- UPDATE


type Msg
    = ReceiveItems Category (Api.Result (List Item))
    | RouteChange Route
    | LoadTextMsg LoadText.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveItems category items ->
            let
                newItems =
                    insertItems category (RemoteData.fromResult items) model.items
            in
                { model
                    | items = newItems
                    , loadText = LoadText.toggle False model.loadText
                }
                    ! []

        RouteChange route ->
            updateRoute model route

        LoadTextMsg childMsg ->
            let
                loadText =
                    LoadText.update childMsg model.loadText
            in
                { model | loadText = loadText } ! []


updateRoute : Model -> Route -> ( Model, Cmd Msg )
updateRoute model route =
    case route of
        Router.View category ->
            let
                ( items, cmd, loading ) =
                    if RemoteData.isDone <| getData category model.items then
                        ( model.items, Cmd.none, False )
                    else
                        ( insertItems category Loading model.items
                        , fetchItems category
                        , True
                        )
            in
                ( { model
                    | items = items
                    , category = category
                    , loadText = LoadText.toggle loading model.loadText
                  }
                , cmd
                )

        _ ->
            model ! []


fetchItems : Category -> Cmd Msg
fetchItems category =
    Api.send (ReceiveItems category) << Api.requestCategory <| category


insertItems : Category -> RemoteItems -> ItemsCache -> ItemsCache
insertItems category =
    Dict.insert (Api.stringId category)


getData : Category -> ItemsCache -> RemoteItems
getData category =
    Maybe.withDefault NotRequested << Dict.get (Api.stringId category)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map LoadTextMsg <| LoadText.subscriptions model.loadText

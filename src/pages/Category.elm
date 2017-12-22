module Pages.Category exposing (..)

import Api exposing (Category)
import Dict exposing (Dict)
import Elements
import Html.Styled exposing (..)
import ItemEntry
import LoadText
import PageTitle
import RemoteData exposing (RemoteData(..), WebData)
import Router exposing (Route)
import Types.Item exposing (Item)


-- MODEL


type alias Model =
    { category : Category
    , items : ItemsCache
    , loadText : LoadText.Model
    }


type alias ItemsCache =
    Dict String WebItems


type alias WebItems =
    WebData (List Item)


init : Route -> ( Model, Cmd Msg )
init route =
    updateRoute (Model Api.Best Dict.empty (LoadText.init False)) route



-- VIEW


view : Model -> Html msg
view model =
    case getData model.category model.items of
        Success items ->
            section [] <| List.indexedMap viewCategoryItem items

        Loading ->
            LoadText.view model.loadText

        _ ->
            text "There doesn't seem to be anything here."


viewCategoryItem : Int -> Item -> Html msg
viewCategoryItem rank item =
    Elements.categoryItem []
        [ Elements.itemRank [] [ text <| "#" ++ (toString <| 1 + rank) ]
        , ItemEntry.view False [ ItemEntry.By, ItemEntry.Score, ItemEntry.Comments ] item
        ]



-- UPDATE


type Msg
    = ReceiveItems Category (WebData (List Item))
    | RouteChange Route
    | LoadTextMsg LoadText.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveItems category items ->
            let
                newItems =
                    insertItems category items model.items
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
                    if RemoteData.isSuccess <| getData category model.items then
                        ( model.items, Cmd.none, False )
                    else
                        ( insertItems category Loading model.items
                        , fetchItems category
                        , True
                        )
            in
                { model
                    | items = items
                    , category = category
                    , loadText = LoadText.toggle loading model.loadText
                }
                    ! [ cmd
                      , PageTitle.set <| Api.label category
                      ]

        _ ->
            model ! []


fetchItems : Category -> Cmd Msg
fetchItems category =
    Api.send (ReceiveItems category) << Api.requestCategory <| category


insertItems : Category -> WebItems -> ItemsCache -> ItemsCache
insertItems category =
    Dict.insert (Api.stringId category)


getData : Category -> ItemsCache -> WebItems
getData category =
    Maybe.withDefault NotAsked << Dict.get (Api.stringId category)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map LoadTextMsg <| LoadText.subscriptions model.loadText

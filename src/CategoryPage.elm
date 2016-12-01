module CategoryPage exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Item exposing (Item)
import ItemEntry
import RemoteData exposing (RemoteData(..))
import Router exposing (Route)
import Api exposing (Category)


-- MODEL


type alias Model =
    { category : Category
    , items : ItemsCache
    }


type alias ItemsCache =
    Dict String RemoteItems


type alias RemoteItems =
    RemoteData (List Item)


init : Route -> ( Model, Cmd Msg )
init route =
    updateRoute (Model Api.Best Dict.empty) route



-- VIEW


view : Model -> Html msg
view model =
    case getData model.category model.items of
        Done items ->
            section [] <| List.indexedMap viewCategoryItem items

        Loading ->
            text "Loading..."

        _ ->
            text "There doesn't seem to be anything here."


viewCategoryItem : Int -> Item -> Html msg
viewCategoryItem rank item =
    div [ Attr.class "category-item" ]
        [ div [ Attr.class "item-rank" ] [ text <| "#" ++ (toString <| 1 + rank) ]
        , ItemEntry.view False item
        ]



-- UPDATE


type Msg
    = RecieveItems Category (Api.Result (List Item))
    | RouteChange Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecieveItems category items ->
            let
                newItems =
                    insertItems category (RemoteData.fromResult items) model.items
            in
                { model | items = newItems } ! []

        RouteChange route ->
            updateRoute model route


updateRoute : Model -> Route -> ( Model, Cmd Msg )
updateRoute model route =
    case route of
        Router.View category ->
            let
                ( items, cmd ) =
                    if RemoteData.isDone <| getData category model.items then
                        model.items ! []
                    else
                        ( insertItems category Loading model.items
                        , fetchItems category
                        )
            in
                ( { model | items = items, category = category }
                , cmd
                )

        _ ->
            model ! []


fetchItems : Category -> Cmd Msg
fetchItems category =
    Api.send (RecieveItems category) << Api.requestCategory <| category


insertItems : Category -> RemoteItems -> ItemsCache -> ItemsCache
insertItems category =
    Dict.insert (Api.stringId category)


getData : Category -> ItemsCache -> RemoteItems
getData category =
    Maybe.withDefault NotRequested << Dict.get (Api.stringId category)

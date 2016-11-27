module App exposing (Model, Msg(RouteChange), init, update, view, subscriptions)

import Api exposing (Category)
import CategoryPage
import Dict exposing (Dict)
import Header
import Html exposing (..)
import Html.Attributes as Attr
import Item exposing (Item)
import Nav
import Navigation exposing (Location)
import NotFoundPage
import RemoteData exposing (..)
import Router exposing (Route)
import Util exposing (..)


-- MODEL


type alias Model =
    { currentRoute : Route
    , items : ItemsCache
    }


type alias ItemsCache =
    Dict String RemoteItems


type alias RemoteItems =
    RemoteData (List Item)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Router.parseLocation location

        ( items, cmd ) =
            updateRoute Dict.empty route
    in
        ( { currentRoute = route, items = items }, cmd )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Attr.class "container" ]
        [ Header.view
        , Nav.view model.currentRoute
        , viewMain model
        ]


viewMain : Model -> Html Msg
viewMain model =
    let
        content =
            case model.currentRoute of
                Router.NotFound ->
                    NotFoundPage.view

                Router.View category ->
                    CategoryPage.view <| getData category model.items
    in
        main_ [] [ content ]



-- UPDATE


type Msg
    = FetchItems Category RemoteItems
    | RouteChange Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchItems category items ->
            let
                newItems =
                    insertItems category items model.items
            in
                { model | items = newItems } ! []

        RouteChange route ->
            let
                ( newItems, cmd ) =
                    updateRoute model.items route
            in
                ( { model | items = newItems, currentRoute = route }, cmd )


updateRoute : ItemsCache -> Route -> ( ItemsCache, Cmd Msg )
updateRoute items route =
    case route of
        Router.View category ->
            if RemoteData.isDone <| getData category items then
                items ! []
            else
                ( insertItems category Loading items, fetchItems category )

        _ ->
            items ! []


fetchItems : Category -> Cmd Msg
fetchItems category =
    Api.send (FetchItems category) << Api.requestCategory <| category


insertItems : Category -> RemoteItems -> ItemsCache -> ItemsCache
insertItems category =
    Dict.insert (Api.stringId category)


getData : Category -> ItemsCache -> RemoteItems
getData category =
    Maybe.withDefault NotRequested << Dict.get (Api.stringId category)



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none

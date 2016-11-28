module App exposing (Model, Msg(RouteChange), init, update, view, subscriptions)

import Api exposing (Category)
import CategoryPage
import Dict exposing (Dict)
import Header
import Html exposing (..)
import Html.Attributes as Attr
import Item exposing (Item)
import ItemPage
import Nav
import Navigation exposing (Location)
import NotFoundPage
import RemoteData exposing (..)
import Router exposing (Route)


-- MODEL


type alias Model =
    { currentRoute : Route
    , items : ItemsCache
    , itemPage : ItemPage.Model
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

        ( itemPage, itemPageCmd ) =
            ItemPage.init route
    in
        { currentRoute = route
        , items = items
        , itemPage = itemPage
        }
            ! [ cmd
              , Cmd.map ItemPageMsg itemPageCmd
              ]



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

                Router.ViewItem id ->
                    Html.map ItemPageMsg <| ItemPage.view model.itemPage
    in
        main_ [] [ content ]



-- UPDATE


type Msg
    = RecieveItems Category (Api.Result (List Item))
    | RouteChange Route
    | ItemPageMsg ItemPage.Msg


{-| TODO: Move ViewCategory logic to CategoryPage
-}
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
            let
                ( newItems, cmd ) =
                    updateRoute model.items route

                ( itemPage, itemPageCmd ) =
                    ItemPage.update (ItemPage.RouteChange route) model.itemPage
            in
                { model
                    | currentRoute = route
                    , items = newItems
                    , itemPage = itemPage
                }
                    ! [ cmd
                      , Cmd.map ItemPageMsg itemPageCmd
                      ]

        ItemPageMsg childMsg ->
            let
                ( newModel, cmd ) =
                    ItemPage.update childMsg model.itemPage
            in
                ( { model | itemPage = newModel }, Cmd.map ItemPageMsg cmd )


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
    Api.send (RecieveItems category) << Api.requestCategory <| category


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

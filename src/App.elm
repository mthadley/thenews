module App exposing (Model, Msg(RouteChange), init, update, view, subscriptions)

import Api exposing (Category, RemoteData(..))
import Dict exposing (Dict)
import Header
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Item exposing (Item)
import Navigation exposing (Location)
import NotFoundPage
import Router exposing (Route)
import Util exposing (..)


-- MODEL


type alias Model =
    { currentRoute : Route
    , items : Items
    }


type alias Items =
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
                    viewList <| getData category model.items
    in
        main_ [] [ content ]


viewList : RemoteItems -> Html Msg
viewList items =
    case items of
        Done items ->
            section [] <| List.indexedMap viewItem items

        Loading ->
            text "Loading..."

        _ ->
            text "There doesn't seem to be anything here."


viewItem : Int -> Item -> Html Msg
viewItem rank item =
    article [ Attr.class "item" ]
        [ div [ Attr.class "item-rank" ] [ text <| "#" ++ (toString <| 1 + rank) ]
        , div [ Attr.class "item-content" ]
            [ h2 []
                [ a
                    [ Attr.href <| Maybe.withDefault "" item.url
                    ]
                    [ text <| Maybe.withDefault "No Title" item.title ]
                ]
            , footer []
                [ detailString
                    [ ( "By ", Just item.by )
                    , ( "Score: ", maybeToString item.score )
                    , ( "Comments: ", maybeToString item.descendants )
                    ]
                    |> text
                ]
            ]
        ]



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


updateRoute : Items -> Route -> ( Items, Cmd Msg )
updateRoute items route =
    case route of
        Router.View category ->
            case (getData category items) of
                Done _ ->
                    items ! []

                _ ->
                    ( insertItems category Loading items, fetchItems category )

        _ ->
            items ! []


fetchItems : Category -> Cmd Msg
fetchItems category =
    Api.send (FetchItems category) << Api.requestCategory <| category


insertItems : Category -> RemoteItems -> Items -> Items
insertItems category =
    Dict.insert (Api.stringId category)


getData : Category -> Items -> RemoteItems
getData category =
    Maybe.withDefault NotRequested << Dict.get (Api.stringId category)



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none

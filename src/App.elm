module App exposing (Model, Msg(RouteChange), init, update, view, subscriptions)

import CategoryPage
import Header
import Html exposing (..)
import Html.Attributes as Attr
import ItemPage
import Nav
import Navigation exposing (Location)
import NotFoundPage
import Router exposing (Route)


-- MODEL


type alias Model =
    { currentRoute : Route
    , itemPage : ItemPage.Model
    , categoryPage : CategoryPage.Model
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Router.parseLocation location

        ( itemPage, itemPageCmd ) =
            ItemPage.init route

        ( categoryPage, categoryPageCmd ) =
            CategoryPage.init route
    in
        { currentRoute = route
        , itemPage = itemPage
        , categoryPage = categoryPage
        }
            ! [ Cmd.map ItemPageMsg itemPageCmd
              , Cmd.map CategoryPageMsg categoryPageCmd
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
                    Html.map CategoryPageMsg <| CategoryPage.view model.categoryPage

                Router.ViewItem id ->
                    Html.map ItemPageMsg <| ItemPage.view model.itemPage
    in
        main_ [] [ content ]



-- UPDATE


type Msg
    = RouteChange Route
    | CategoryPageMsg CategoryPage.Msg
    | ItemPageMsg ItemPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteChange route ->
            let
                ( itemPage, itemPageCmd ) =
                    ItemPage.update (ItemPage.RouteChange route) model.itemPage

                ( categoryPage, categoryPageCmd ) =
                    CategoryPage.update (CategoryPage.RouteChange route) model.categoryPage
            in
                { model
                    | currentRoute = route
                    , categoryPage = categoryPage
                    , itemPage = itemPage
                }
                    ! [ Cmd.map CategoryPageMsg categoryPageCmd
                      , Cmd.map ItemPageMsg itemPageCmd
                      ]

        ItemPageMsg childMsg ->
            let
                ( newModel, cmd ) =
                    ItemPage.update childMsg model.itemPage
            in
                ( { model | itemPage = newModel }, Cmd.map ItemPageMsg cmd )

        CategoryPageMsg childMsg ->
            let
                ( newModel, cmd ) =
                    CategoryPage.update childMsg model.categoryPage
            in
                ( { model | categoryPage = newModel }, Cmd.map CategoryPageMsg cmd )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none

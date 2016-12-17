module App exposing (Model, Msg(RouteChange), init, update, view, subscriptions)

import Header
import Html exposing (..)
import Html.Attributes as Attr
import Nav
import Pages.Category as CategoryPage
import Pages.Item as ItemPage
import Pages.NotFound as NotFoundPage
import Pages.User as UserPage
import Router exposing (Route)


-- MODEL


type alias Model =
    { currentRoute : Route
    , itemPage : ItemPage.Model
    , categoryPage : CategoryPage.Model
    , userPage : UserPage.Model
    }


init : Route -> ( Model, Cmd Msg )
init route =
    let
        ( itemPage, itemPageCmd ) =
            ItemPage.init route

        ( categoryPage, categoryPageCmd ) =
            CategoryPage.init route

        ( userPage, userPageCmd ) =
            UserPage.init route
    in
        { currentRoute = route
        , itemPage = itemPage
        , categoryPage = categoryPage
        , userPage = userPage
        }
            ! [ Cmd.map ItemPageMsg itemPageCmd
              , Cmd.map CategoryPageMsg categoryPageCmd
              , Cmd.map UserPageMsg userPageCmd
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

                Router.View _ ->
                    Html.map CategoryPageMsg <| CategoryPage.view model.categoryPage

                Router.ViewItem _ ->
                    Html.map ItemPageMsg <| ItemPage.view model.itemPage

                Router.ViewUser _ ->
                    Html.map UserPageMsg <| UserPage.view model.userPage
    in
        main_ [] [ content ]



-- UPDATE


type Msg
    = RouteChange Route
    | CategoryPageMsg CategoryPage.Msg
    | ItemPageMsg ItemPage.Msg
    | UserPageMsg UserPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteChange route ->
            let
                ( itemPage, itemPageCmd ) =
                    ItemPage.update (ItemPage.RouteChange route) model.itemPage

                ( categoryPage, categoryPageCmd ) =
                    CategoryPage.update (CategoryPage.RouteChange route) model.categoryPage

                ( userPage, userPageCmd ) =
                    UserPage.update (UserPage.RouteChange route) model.userPage
            in
                { model
                    | currentRoute = route
                    , categoryPage = categoryPage
                    , itemPage = itemPage
                    , userPage = userPage
                }
                    ! [ Cmd.map CategoryPageMsg categoryPageCmd
                      , Cmd.map ItemPageMsg itemPageCmd
                      , Cmd.map UserPageMsg userPageCmd
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

        UserPageMsg childMsg ->
            let
                ( newModel, cmd ) =
                    UserPage.update childMsg model.userPage
            in
                ( { model | userPage = newModel }, Cmd.map UserPageMsg cmd )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map CategoryPageMsg <|
            CategoryPage.subscriptions model.categoryPage
        , Sub.map UserPageMsg <|
            UserPage.subscriptions model.userPage
        , Sub.map ItemPageMsg <|
            ItemPage.subscriptions model.itemPage
        ]

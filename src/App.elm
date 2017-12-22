module App exposing (Model, Msg(RouteChange), init, update, view, subscriptions)

import Animation
import Animation.Messenger
import Animation.Spring.Presets as Presets
import Elements
import Header
import Html as UnstyledHtml
import Html.Styled as Html exposing (..)
import Nav
import Pages.Category as CategoryPage
import Pages.Item as ItemPage
import Pages.NotFound as NotFoundPage
import Pages.User as UserPage
import PageTitle
import Router exposing (Route)
import Styles exposing (styles)


-- MODEL


type alias Model =
    { currentRoute : Route
    , nextRoute : Route
    , itemPage : ItemPage.Model
    , categoryPage : CategoryPage.Model
    , userPage : UserPage.Model
    , style : Animation.Messenger.State Msg
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

        style =
            Animation.style <| animationProps In
    in
        { currentRoute = route
        , nextRoute = route
        , itemPage = itemPage
        , categoryPage = categoryPage
        , userPage = userPage
        , style = style
        }
            ! [ Cmd.map ItemPageMsg itemPageCmd
              , Cmd.map CategoryPageMsg categoryPageCmd
              , Cmd.map UserPageMsg userPageCmd
              , maybeSetNotFoundTitle route
              ]



-- VIEW


view : Model -> Html Msg
view model =
    Elements.container []
        [ styles
        , Header.view
        , Nav.view model.nextRoute
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
        fromUnstyled <|
            UnstyledHtml.main_ (Animation.render model.style)
                [ toUnstyled content
                ]



-- UPDATE


type Msg
    = ActivateRoute Route
    | AnimationMsg Animation.Msg
    | RouteChange Route
    | CategoryPageMsg CategoryPage.Msg
    | ItemPageMsg ItemPage.Msg
    | UserPageMsg UserPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteChange route ->
            if route == model.currentRoute then
                model ! []
            else
                { model
                    | nextRoute = route
                    , style =
                        Animation.interrupt
                            [ animate Out
                            , Animation.Messenger.send <| ActivateRoute route
                            ]
                            model.style
                }
                    ! [ maybeSetNotFoundTitle route ]

        ActivateRoute route ->
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
                    , style = Animation.interrupt [ animate In ] model.style
                }
                    ! [ Cmd.map CategoryPageMsg categoryPageCmd
                      , Cmd.map ItemPageMsg itemPageCmd
                      , Cmd.map UserPageMsg userPageCmd
                      ]

        AnimationMsg animationMsg ->
            let
                ( style, cmd ) =
                    Animation.Messenger.update animationMsg model.style
            in
                ( { model | style = style }, cmd )

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


type Animate
    = In
    | Out


animate : Animate -> Animation.Messenger.Step Msg
animate =
    Animation.toWith (Animation.spring Presets.zippy) << animationProps


animationProps : Animate -> List Animation.Property
animationProps animate =
    case animate of
        In ->
            [ Animation.opacity 1
            , Animation.translate (Animation.px 0) (Animation.px 0)
            ]

        Out ->
            [ Animation.opacity 0
            , Animation.translate (Animation.px 0) (Animation.px 12)
            ]


maybeSetNotFoundTitle : Route -> Cmd msg
maybeSetNotFoundTitle route =
    case route of
        Router.NotFound ->
            PageTitle.set "404 Not Found"

        _ ->
            Cmd.none



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
        , Animation.subscription AnimationMsg [ model.style ]
        ]

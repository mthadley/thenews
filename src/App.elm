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
import Store exposing (Store, Action)
import Styles exposing (styles)


-- MODEL


type alias Model =
    { currentRoute : Route
    , nextRoute : Route
    , page : Page
    , store : Store
    , style : Animation.Messenger.State Msg
    }


type Page
    = CategoryPage CategoryPage.Model
    | UserPage UserPage.Model
    | ItemPage ItemPage.Model
    | NotFoundPage


init : Route -> ( Model, Cmd Msg )
init route =
    let
        ( page, cmd, store ) =
            initPage route Store.init

        style =
            Animation.style <| animationProps In
    in
        { currentRoute = route
        , nextRoute = route
        , page = page
        , style = style
        , store = store
        }
            ! [ cmd
              , maybeSetNotFoundTitle route
              ]


initPage : Route -> Store -> ( Page, Cmd Msg, Store )
initPage route store =
    let
        ( page, pageCmd, action ) =
            case route of
                Router.View category ->
                    let
                        ( model, pageCmd, action ) =
                            CategoryPage.init category
                    in
                        ( CategoryPage model
                        , pageCmd
                        , Store.map CategoryPageMsg action
                        )

                Router.ViewItem id ->
                    let
                        ( model, action ) =
                            ItemPage.init store id
                    in
                        ( ItemPage model, Cmd.none, Store.map ItemPageMsg action )

                Router.ViewUser user ->
                    let
                        ( model, pageCmd, action ) =
                            UserPage.init user
                    in
                        ( UserPage model, pageCmd, Store.map UserPageMsg action )

                _ ->
                    ( NotFoundPage, Cmd.none, Store.none )

        ( newStore, storeCmd, outCmd ) =
            Store.update action store
    in
        ( page
        , Cmd.batch
            [ pageCmd
            , Cmd.map StoreMsg storeCmd
            , outCmd
            ]
        , newStore
        )



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
viewMain { page, store, style } =
    let
        content =
            case page of
                CategoryPage model ->
                    CategoryPage.view store model

                ItemPage model ->
                    Html.map ItemPageMsg <| ItemPage.view store model

                UserPage model ->
                    UserPage.view store model

                NotFoundPage ->
                    NotFoundPage.view
    in
        fromUnstyled <|
            UnstyledHtml.main_ (Animation.render style)
                [ toUnstyled content
                ]



-- UPDATE


type Msg
    = ActivateRoute Route
    | AnimationMsg Animation.Msg
    | RouteChange Route
    | StoreMsg (Action Msg)
    | CategoryPageMsg CategoryPage.Msg
    | ItemPageMsg ItemPage.Msg
    | UserPageMsg UserPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( RouteChange route, _ ) ->
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

        ( ActivateRoute route, _ ) ->
            let
                ( page, cmd, store ) =
                    initPage route model.store
            in
                ( { model
                    | currentRoute = route
                    , page = page
                    , store = store
                    , style = Animation.interrupt [ animate In ] model.style
                  }
                , cmd
                )

        ( AnimationMsg animationMsg, _ ) ->
            let
                ( style, cmd ) =
                    Animation.Messenger.update animationMsg model.style
            in
                ( { model | style = style }, cmd )

        ( StoreMsg action, _ ) ->
            let
                ( store, cmd, outCmd ) =
                    Store.update action model.store
            in
                ( { model | store = store }
                , Cmd.batch [ Cmd.map StoreMsg cmd, outCmd ]
                )

        ( CategoryPageMsg msg, CategoryPage pageModel ) ->
            let
                ( newPageModel, action ) =
                    CategoryPage.update model.store msg pageModel

                ( store, cmd ) =
                    updateStoreWith CategoryPageMsg action model.store
            in
                ( { model | page = CategoryPage newPageModel, store = store }, cmd )

        ( UserPageMsg msg, UserPage pageModel ) ->
            let
                ( newPageModel, action ) =
                    UserPage.update model.store msg pageModel

                ( store, cmd ) =
                    updateStoreWith UserPageMsg action model.store
            in
                ( { model | page = UserPage newPageModel, store = store }, cmd )

        ( ItemPageMsg msg, ItemPage pageModel ) ->
            let
                ( newPageModel, pageCmd, action ) =
                    ItemPage.update model.store msg pageModel

                ( store, cmd ) =
                    updateStoreWith ItemPageMsg action model.store
            in
                ( { model | page = ItemPage newPageModel, store = store }
                , Cmd.batch [ Cmd.map ItemPageMsg pageCmd, cmd ]
                )

        -- Msgs arriving for the wrong model
        ( _, _ ) ->
            model ! []


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


updateStoreWith : (a -> Msg) -> Action a -> Store -> ( Store, Cmd Msg )
updateStoreWith f action store =
    let
        ( newStore, cmd, outCmd ) =
            Store.update (Store.map f action) store
    in
        ( newStore, Cmd.batch [ Cmd.map StoreMsg cmd, outCmd ] )



-- SUBS


getPageSub : Model -> Sub Msg
getPageSub { page, store } =
    case page of
        CategoryPage model ->
            Sub.map CategoryPageMsg <| CategoryPage.subscriptions store model

        ItemPage model ->
            Sub.map ItemPageMsg <| ItemPage.subscriptions store model

        UserPage model ->
            Sub.map UserPageMsg <| UserPage.subscriptions store model

        NotFoundPage ->
            Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animation.subscription AnimationMsg [ model.style ]
        , getPageSub model
        ]

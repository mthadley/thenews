module Main exposing (main)

import Browser
import Browser.Hash
import Browser.Navigation as Navigation exposing (Key)
import Css exposing (num, px, vh, zero)
import Html as UnstyledHtml
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder)
import Pages.Category as CategoryPage
import Pages.Item as ItemPage
import Pages.NotFound as NotFoundPage
import Pages.User as UserPage
import Router exposing (Route)
import Store exposing (Action, Store)
import Styles exposing (styles)
import Theme exposing (Theme)
import Url exposing (Url)
import Views.Header as Header
import Views.Nav as Nav


main : Program Flags Model Msg
main =
    Browser.Hash.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange << Router.parse
        }



-- FLAGS


type alias Flags =
    { theme : String }



-- MODEL


type alias Model =
    { route : Route
    , page : Page
    , store : Store
    , key : Key
    }


type Page
    = CategoryPage CategoryPage.Model
    | UserPage UserPage.Model
    | ItemPage ItemPage.Model
    | NotFoundPage


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Router.parse url

        ( store, storeCmd ) =
            flags.theme
                |> Theme.fromString
                |> Maybe.withDefault Theme.Dark
                |> Store.init

        ( page, cmd, newStore ) =
            initPage key route store
    in
    ( { route = route
      , key = key
      , page = page
      , store = newStore
      }
    , Cmd.batch [ cmd, Cmd.map StoreMsg storeCmd ]
    )


initPage : Key -> Route -> Store -> ( Page, Cmd Msg, Store )
initPage key route store =
    let
        ( page, action ) =
            case route of
                Router.View category ->
                    let
                        ( model, action_ ) =
                            CategoryPage.init category
                    in
                    ( CategoryPage model
                    , Store.map CategoryPageMsg action_
                    )

                Router.ViewItem id ->
                    let
                        ( model, action_ ) =
                            ItemPage.init store id
                    in
                    ( ItemPage model, Store.map ItemPageMsg action_ )

                Router.ViewUser user ->
                    let
                        ( model, action_ ) =
                            UserPage.init user
                    in
                    ( UserPage model, Store.map UserPageMsg action_ )

                _ ->
                    ( NotFoundPage, Store.none )

        ( newStore, storeCmd, outCmd ) =
            Store.update key action store
    in
    ( page
    , Cmd.batch
        [ Cmd.map StoreMsg storeCmd
        , outCmd
        ]
    , newStore
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        ( title, content ) =
            viewMain model

        theme =
            Store.getTheme model.store
    in
    { title = "TheNews: " ++ title
    , body =
        List.map Html.toUnstyled
            [ styled div
                [ Css.margin2 Css.zero Css.auto
                , Css.maxWidth <| px 768
                , Css.padding <| px 8
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.minHeight (vh 100)
                ]
                []
                [ styles theme
                , div []
                    [ Header.view
                    , Nav.view theme model.route
                    ]
                , styled main_
                    [ Css.flexGrow (num 1) ]
                    []
                    [ content ]
                , viewFooter theme
                ]
            ]
    }


viewMain : Model -> ( String, Html Msg )
viewMain { page, store } =
    case page of
        CategoryPage model ->
            Tuple.mapSecond (Html.map CategoryPageMsg) <| CategoryPage.view store model

        ItemPage model ->
            Tuple.mapSecond (Html.map ItemPageMsg) <| ItemPage.view store model

        UserPage model ->
            Tuple.mapSecond (Html.map UserPageMsg) <| UserPage.view store model

        NotFoundPage ->
            NotFoundPage.view


viewFooter : Theme -> Html msg
viewFooter theme =
    let
        colors =
            Theme.colors theme
    in
    styled footer
        [ Css.padding2 (px 0) zero
        , Css.textAlign Css.center
        , Css.position Css.relative
        , Css.backgroundColor colors.primary
        , Theme.termShadow theme
        , Css.margin3 (px 30) zero (px 20)
        ]
        []
        [ styled a
            [ Css.color colors.secondary ]
            [ Attributes.href "https://github.com/mthadley/thenews" ]
            [ text "Github" ]
        ]



-- UPDATE


type Msg
    = StoreMsg (Action Msg)
      -- Routing
    | UrlRequest Browser.UrlRequest
    | UrlChange Route
      -- Page Msgs
    | CategoryPageMsg CategoryPage.Msg
    | ItemPageMsg ItemPage.Msg
    | UserPageMsg UserPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model
                    , Router.redirectExternal model.key href
                    )

        ( UrlChange route, _ ) ->
            let
                ( page, cmd, store ) =
                    initPage model.key route model.store
            in
            ( { model
                | route = route
                , page = page
                , store = store
              }
            , cmd
            )

        ( StoreMsg action, _ ) ->
            let
                ( store, cmd, outCmd ) =
                    Store.update model.key action model.store
            in
            ( { model | store = store }
            , Cmd.batch [ Cmd.map StoreMsg cmd, outCmd ]
            )

        ( CategoryPageMsg pageMsg, CategoryPage pageModel ) ->
            let
                ( newPageModel, action ) =
                    CategoryPage.update model.store pageMsg pageModel

                ( store, cmd ) =
                    updateStoreWith model.key CategoryPageMsg action model.store
            in
            ( { model | page = CategoryPage newPageModel, store = store }, cmd )

        ( UserPageMsg pageMsg, UserPage pageModel ) ->
            let
                ( newPageModel, action ) =
                    UserPage.update model.store pageMsg pageModel

                ( store, cmd ) =
                    updateStoreWith model.key UserPageMsg action model.store
            in
            ( { model | page = UserPage newPageModel, store = store }, cmd )

        ( ItemPageMsg pageMsg, ItemPage pageModel ) ->
            let
                ( newPageModel, action ) =
                    ItemPage.update model.store pageMsg pageModel

                ( store, cmd ) =
                    updateStoreWith model.key ItemPageMsg action model.store
            in
            ( { model | page = ItemPage newPageModel, store = store }, cmd )

        -- Msgs arriving for the wrong model
        ( _, _ ) ->
            ( model, Cmd.none )


updateStoreWith : Key -> (a -> Msg) -> Action a -> Store -> ( Store, Cmd Msg )
updateStoreWith key f action store =
    let
        ( newStore, cmd, outCmd ) =
            Store.update key (Store.map f action) store
    in
    ( newStore, Cmd.batch [ Cmd.map StoreMsg cmd, outCmd ] )



-- SUBS


pageSubs : Model -> Sub Msg
pageSubs { page, store } =
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
        [ pageSubs model
        , Sub.map StoreMsg Store.subscriptions
        ]

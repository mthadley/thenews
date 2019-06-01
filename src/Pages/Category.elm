module Pages.Category exposing (Model, Msg(..), getCategoryItems, init, subscriptions, update, view, viewCategoryItem)

import Css exposing (px)
import Data.Category as Category exposing (Category)
import Data.Item as Item exposing (Item)
import Html.Styled exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Store exposing (Action, Store)
import Time
import Views.Item as ItemView
import Views.LoadText as LoadText



-- MODEL


type alias Model =
    { category : Category
    , loadText : LoadText.Model
    }


init : Category -> ( Model, Action Msg )
init category =
    ( { category = category
      , loadText = LoadText.init
      }
    , Store.tag RecieveCategory <| Store.requestCategory category
    )



-- VIEW


view : Store -> Model -> ( String, Html Msg )
view store model =
    case getCategoryItems model.category store of
        Success items ->
            ( Category.toString model.category
            , section [] <| List.indexedMap (viewCategoryItem (Store.getZone store)) items
            )

        Loading ->
            ( "Loading...", LoadText.view model.loadText )

        _ ->
            ( "Nothing...", text "There doesn't seem to be anything here." )


viewCategoryItem : Time.Zone -> Int -> Item -> Html Msg
viewCategoryItem zone rank item =
    styled div
        [ Css.displayFlex ]
        []
        [ styled div
            [ Css.flexShrink Css.zero
            , Css.fontSize (px 18)
            , Css.width (px 48)
            ]
            []
            [ text <| "#" ++ (String.fromInt <| 1 + rank) ]
        , ItemView.view
            [ ItemView.by
            , ItemView.score
            , ItemView.comments
            ]
            item
        ]



-- UPDATE


type Msg
    = LoadTextMsg LoadText.Msg
    | RecieveCategory
    | ExternalLink String


update : Store -> Msg -> Model -> ( Model, Action Msg )
update store msg model =
    case msg of
        LoadTextMsg childMsg ->
            let
                loadText =
                    LoadText.update childMsg model.loadText
            in
            ( { model | loadText = loadText }, Store.none )

        RecieveCategory ->
            Store.getCategory store model.category
                |> RemoteData.withDefault []
                |> List.map Store.requestItem
                |> Store.batch
                |> Tuple.pair model

        ExternalLink href ->
            ( model, Store.navigate href )


getCategoryItems : Category -> Store -> WebData (List Item)
getCategoryItems category store =
    Store.getCategory store category
        |> RemoteData.andThen (Store.getItems store)


subscriptions : Store -> Model -> Sub Msg
subscriptions store model =
    getCategoryItems model.category store
        |> RemoteData.isLoading
        |> LoadText.subscriptions
        |> Sub.map LoadTextMsg

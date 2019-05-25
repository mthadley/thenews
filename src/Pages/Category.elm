module Pages.Category exposing (Model, Msg(..), getCategoryItems, init, subscriptions, update, view, viewCategoryItem)

import Api exposing (Category)
import Elements
import Html.Styled exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Store exposing (Action, Store)
import Types.Item as Item exposing (Item)
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


view : Store -> Model -> ( String, Html msg )
view store model =
    case getCategoryItems model.category store of
        Success items ->
            ( Api.label model.category
            , section [] <| List.indexedMap viewCategoryItem items
            )

        Loading ->
            ( "Loading...", LoadText.view model.loadText )

        _ ->
            ( "Nothing...", text "There doesn't seem to be anything here." )


viewCategoryItem : Int -> Item -> Html msg
viewCategoryItem rank item =
    Elements.categoryItem []
        [ Elements.itemRank [] [ text <| "#" ++ (String.fromInt <| 1 + rank) ]
        , ItemView.view [ ItemView.by, ItemView.score, ItemView.comments ] item
        ]



-- UPDATE


type Msg
    = LoadTextMsg LoadText.Msg
    | RecieveCategory


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

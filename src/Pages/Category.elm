module Pages.Category exposing (..)

import Api exposing (Category)
import Elements
import Html.Styled exposing (..)
import PageTitle
import RemoteData exposing (RemoteData(..), WebData)
import Store exposing (Store, Action)
import Types.Item exposing (Item)
import Views.Item as ItemView
import Views.LoadText as LoadText


-- MODEL


type alias Model =
    { category : Category
    , loadText : LoadText.Model
    }


init : Category -> ( Model, Cmd msg, Action Msg )
init category =
    ( { category = category
      , loadText = LoadText.init
      }
    , PageTitle.set <| Api.label category
    , Store.tag RecieveCategory <| Store.requestCategory category
    )



-- VIEW


view : Store -> Model -> Html msg
view store model =
    case getCategoryItems model.category store of
        Success items ->
            section [] <| List.indexedMap viewCategoryItem items

        Loading ->
            LoadText.view model.loadText

        _ ->
            text "There doesn't seem to be anything here."


viewCategoryItem : Int -> Item -> Html msg
viewCategoryItem rank item =
    Elements.categoryItem []
        [ Elements.itemRank [] [ text <| "#" ++ (toString <| 1 + rank) ]
        , ItemView.view False [ ItemView.By, ItemView.Score, ItemView.Comments ] item
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
                |> (,) model


getCategoryItems : Category -> Store -> WebData (List Item)
getCategoryItems category store =
    Store.getCategory store category
        |> RemoteData.andThen (Store.getItems store)


subscriptions : Store -> Model -> Sub Msg
subscriptions store model =
    Store.getCategory store model.category
        |> RemoteData.isLoading
        |> LoadText.subscriptions
        |> Sub.map LoadTextMsg

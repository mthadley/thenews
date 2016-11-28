module CategoryPage exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Util exposing (maybeToString)
import Item exposing (Item)
import ItemEntry
import RemoteData exposing (RemoteData(..))


view : RemoteData (List Item) -> Html msg
view items =
    case items of
        Done items ->
            section [] <| List.indexedMap viewCategoryItem items

        Loading ->
            text "Loading..."

        _ ->
            text "There doesn't seem to be anything here."


viewCategoryItem : Int -> Item -> Html msg
viewCategoryItem rank item =
    div [ Attr.class "category-item" ]
        [ div [ Attr.class "item-rank" ] [ text <| "#" ++ (toString <| 1 + rank) ]
        , ItemEntry.view False item
        ]

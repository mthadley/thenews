module CategoryPage exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Util exposing (detailString, maybeToString)
import Item exposing (Item)
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

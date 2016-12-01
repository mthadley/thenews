module ItemEntry exposing (..)

import Item exposing (Item, Type(..))
import Html exposing (..)
import Html.Attributes as Attr
import Router
import Util exposing (maybeToString, viewHtmlContent)


view : Bool -> Item -> Html msg
view showText item =
    let
        textContent =
            if showText then
                Maybe.withDefault Util.empty <|
                    Maybe.map viewHtmlContent item.text
            else
                Util.empty
    in
        article [ Attr.class "item" ]
            [ h2 [] [ spanOrLink item.url <| getTitle item ]
            , textContent
            , footer [] <|
                viewDetails
                    [ ( "By "
                      , Just item.by
                      , Just <| Router.reverse <| Router.ViewUser item.by
                      )
                    , ( "Score: "
                      , maybeToString item.score
                      , Nothing
                      )
                    , ( "Comments: "
                      , maybeToString item.descendants
                      , Just <| Router.reverse <| Router.ViewItem item.id
                      )
                    ]
            ]


viewDetails : List ( String, Maybe String, Maybe String ) -> List (Html msg)
viewDetails =
    let
        detail ( name, value, href ) =
            Maybe.map (spanOrLink href << (++) name) value
    in
        List.intersperse (text " • ") << List.filterMap detail


getTitle : Item -> String
getTitle item =
    case item.type_ of
        Comment ->
            "Comment"

        PollOption ->
            "Poll Option"

        _ ->
            Maybe.withDefault "Untitled" item.title


spanOrLink : Maybe String -> String -> Html msg
spanOrLink href content =
    if href == Nothing then
        span [] [ text content ]
    else
        a [ Attr.href <| Maybe.withDefault "#" href ] [ text content ]

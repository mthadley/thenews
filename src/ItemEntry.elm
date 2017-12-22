module ItemEntry exposing (DetailType(..), view)

import DateFormat
import Elements
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Router
import Types.Item exposing (Item, Type(..))
import Util exposing (maybeToString)


type DetailType
    = By
    | Score
    | Comments
    | Created


type alias Detail =
    ( String, Maybe String, Maybe String )


view : Bool -> List DetailType -> Item -> Html msg
view showText detailTypes item =
    Elements.item []
        [ Elements.itemHeader []
            [ spanOrLink item.url <| getTitle item ]
        , Util.viewIf showText <|
            Util.viewMaybe Util.viewHtmlContent item.text
        , footer [] <| viewDetails item detailTypes
        ]


getDetail : Item -> DetailType -> Detail
getDetail item type_ =
    case type_ of
        By ->
            ( "By "
            , Just item.by
            , Just <| Router.reverse <| Router.ViewUser item.by
            )

        Score ->
            ( "Score: "
            , maybeToString item.score
            , Nothing
            )

        Comments ->
            ( "Comments: "
            , maybeToString item.descendants
            , Just <| Router.reverse <| Router.ViewItem item.id
            )

        Created ->
            ( ""
            , Just <| DateFormat.format item.time
            , Nothing
            )


viewDetails : Item -> List DetailType -> List (Html msg)
viewDetails item =
    let
        detail ( name, value, href ) =
            Maybe.map (spanOrLink href << (++) name) value
    in
        List.intersperse (text " • ") << List.filterMap (detail << getDetail item)


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

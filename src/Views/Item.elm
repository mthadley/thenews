module Views.Item exposing (by, comments, created, score, textContent, view)

import Elements
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Router
import Tagged
import Types.Item exposing (Item, Type(..))
import Util.DateFormat as DateFormat
import Util.Html exposing (viewHtmlContent, viewIf, viewMaybe)
import Util.Json exposing (maybeToString)


type DetailType
    = By
    | Score
    | Comments
    | Created
    | TextContent


by : DetailType
by =
    By


comments : DetailType
comments =
    Comments


created : DetailType
created =
    Created


score : DetailType
score =
    Score


textContent : DetailType
textContent =
    TextContent


type alias Detail =
    ( String, Maybe String, Maybe String )


view : List DetailType -> Item -> Html msg
view details item =
    Elements.item []
        [ Elements.itemHeader []
            [ spanOrLink item.url <| getTitle item ]
        , viewIf (List.member TextContent details) <|
            viewMaybe viewHtmlContent item.text
        , footer [] <| viewDetails item details
        ]


getDetail : Item -> DetailType -> Detail
getDetail item type_ =
    case type_ of
        By ->
            ( "By "
            , Just <| Tagged.untag item.by
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

        _ ->
            ( "", Nothing, Nothing )


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

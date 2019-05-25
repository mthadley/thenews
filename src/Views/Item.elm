module Views.Item exposing (by, comments, created, score, textContent, view)

import Data.Item exposing (Item, Type(..))
import Elements
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Router
import Store exposing (Store)
import Tagged
import Time
import Util.DateFormat as DateFormat
import Util.Html exposing (viewHtmlContent, viewIf, viewMaybe)


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


view : Time.Zone -> List DetailType -> Item -> Html msg
view zone details item =
    Elements.item []
        [ Elements.itemHeader []
            [ spanOrLink item.url <| getTitle item ]
        , viewIf (List.member TextContent details) <|
            viewMaybe viewHtmlContent item.text
        , footer [] <| viewDetails zone item details
        ]


getDetail : Time.Zone -> Item -> DetailType -> Detail
getDetail zone item type_ =
    case type_ of
        By ->
            ( "By "
            , Just <| Tagged.untag item.by
            , Just <| Router.reverse <| Router.ViewUser item.by
            )

        Score ->
            ( "Score: "
            , Maybe.map String.fromInt item.score
            , Nothing
            )

        Comments ->
            ( "Comments: "
            , Maybe.map String.fromInt item.descendants
            , Just <| Router.reverse <| Router.ViewItem item.id
            )

        Created ->
            ( ""
            , Just <| DateFormat.format zone item.time
            , Nothing
            )

        _ ->
            ( "", Nothing, Nothing )


viewDetails : Time.Zone -> Item -> List DetailType -> List (Html msg)
viewDetails zone item =
    let
        detail ( name, value, href ) =
            Maybe.map (spanOrLink href << (++) name) value
    in
    List.intersperse (text " • ") << List.filterMap (detail << getDetail zone item)


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
spanOrLink maybeHref content =
    case maybeHref of
        Just href ->
            a [ Attr.href href ] [ text content ]

        Nothing ->
            span [] [ text content ]

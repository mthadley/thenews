module Views.Item exposing (by, comments, created, score, textContent, view)

import Css exposing (px)
import Data.Item exposing (Item, Type(..))
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Router
import Store exposing (Store)
import Tagged
import Time
import Util.DateFormat as DateFormat
import Util.Html exposing (viewHtmlContent, viewIf, viewMaybe)


type DetailType msg
    = By
    | Score
    | Comments
    | Created Time.Zone
    | TextContent (String -> msg)


by : DetailType msg
by =
    By


comments : DetailType msg
comments =
    Comments


created : Time.Zone -> DetailType msg
created =
    Created


score : DetailType msg
score =
    Score


textContent : (String -> msg) -> DetailType msg
textContent =
    TextContent


view : List (DetailType msg) -> Item -> Html msg
view details item =
    styled article
        [ Css.marginBottom <| px 16 ]
        []
        [ styled h2
            [ Css.margin3 Css.zero Css.zero <| px 12
            ]
            []
            [ a [ Attr.href item.url ] [ text (getTitle item) ] ]
        , viewContent item.text details
        , footer [] <| viewDetails item details
        ]


viewContent : Maybe String -> List (DetailType msg) -> Html msg
viewContent maybeText details =
    let
        maybeToMsg =
            List.head <|
                List.filterMap
                    (\detail ->
                        case detail of
                            TextContent toMsg ->
                                Just toMsg

                            _ ->
                                Nothing
                    )
                    details
    in
    Maybe.map2 (\toMsg text -> viewHtmlContent toMsg text) maybeToMsg maybeText
        |> Maybe.withDefault Util.Html.empty


getDetail : Item -> DetailType msg -> ( String, Maybe String, Maybe String )
getDetail item type_ =
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

        Created zone ->
            ( ""
            , Just <| DateFormat.format zone item.time
            , Nothing
            )

        TextContent _ ->
            ( "", Nothing, Nothing )


viewDetails : Item -> List (DetailType msg) -> List (Html msg)
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
spanOrLink maybeHref content =
    case maybeHref of
        Just href ->
            a [ Attr.href href ] [ text content ]

        Nothing ->
            span [] [ text content ]

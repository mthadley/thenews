module ItemPage exposing (Model, Msg(RouteChange), init, update, view)

import Api
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Item exposing (Item)
import ItemEntry
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..))
import Router exposing (Route)
import Util exposing (empty)


-- MODEL


pageSize : Int
pageSize =
    10


type alias Comment =
    { showCount : Int
    , loading : Bool
    , item : Item
    }


type alias Comments =
    Dict Int Comment


type alias Model =
    { comments : Comments
    , item : RemoteData Item
    , showCount : Int
    , loading : Bool
    }


init : Route -> ( Model, Cmd Msg )
init route =
    let
        ( item, cmd ) =
            case route of
                Router.ViewItem id ->
                    ( Loading, fetchItem id )

                _ ->
                    ( NotRequested, Cmd.none )
    in
        { comments = Dict.empty
        , item = item
        , loading = True
        , showCount = 0
        }
            ! [ cmd ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.item of
        Done item ->
            div []
                [ ItemEntry.view True item
                , viewCommentsContainer model item
                ]

        Loading ->
            text "Loading..."

        _ ->
            text "There doesn't seem to be anything here."


viewCommentsContainer : Model -> Item -> Html Msg
viewCommentsContainer { comments, loading, showCount } item =
    case item.kids of
        Just kids ->
            let
                count =
                    List.length kids

                delta =
                    List.length kids - showCount
            in
                section []
                    [ h3 []
                        [ text <|
                            (getCommentsTitle item.type_)
                                ++ " ("
                                ++ (toString count)
                                ++ (getReplyText count)
                                ++ ", "
                                ++ (toString <| Maybe.withDefault 0 item.descendants)
                                ++ " Total"
                                ++ ")"
                        ]
                    , viewComments comments <| List.take showCount kids
                    , viewShowMore item.id delta loading
                    ]

        Nothing ->
            empty


viewComments : Comments -> List Int -> Html Msg
viewComments comments ids =
    let
        viewHelper id =
            Dict.get id comments
                |> Maybe.map (viewComment comments)
                |> Maybe.withDefault empty
    in
        div [ Attr.class "comment-level" ] <| List.map viewHelper ids


viewComment : Comments -> Comment -> Html Msg
viewComment comments { showCount, item, loading } =
    let
        kids =
            Maybe.withDefault [] item.kids

        delta =
            List.length kids - showCount
    in
        article [ Attr.class "comment" ]
            [ h3 [ Attr.class "author" ]
                [ a [ Router.linkTo <| Router.ViewUser item.by ]
                    [ text item.by ]
                , small []
                    [ text <| " at "
                    , time [] [ text <| toString item.time ]
                    ]
                ]
            , Maybe.withDefault empty <| Maybe.map Util.viewHtmlContent item.text
            , viewComments comments <| List.take showCount kids
            , viewShowMore item.id delta loading
            ]


viewShowMore : Int -> Int -> Bool -> Html Msg
viewShowMore id count loading =
    if loading then
        span [ Attr.class "show-more" ] [ text "Loading..." ]
    else if count > 0 then
        a
            [ Attr.class "show-more"
            , Util.jsLink
            , onClick <| FetchComments id
            ]
            [ text <| "▬ " ++ (toString count) ++ getReplyText count
            ]
    else
        empty


getCommentsTitle : Item.Type -> String
getCommentsTitle type_ =
    case type_ of
        Item.Comment ->
            "Replies"

        _ ->
            "Comments"


getReplyText : Int -> String
getReplyText =
    Util.pluralize " Reply" " Replies"



-- UPDATE


type Msg
    = FetchComments Int
    | ReceiveItem (Api.Result Item)
    | ReceiveComments Int (Api.Result (List Item))
    | RouteChange Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchComments id ->
            if id == getId model.item then
                fetchItemComment model
            else
                fetchNestedComment id model

        ReceiveItem item ->
            let
                cmd =
                    Result.withDefault Cmd.none <|
                        Result.map (fetchComments model.showCount) item
            in
                ( { model | item = RemoteData.fromResult item }, cmd )

        ReceiveComments id result ->
            let
                delta =
                    if id == getId model.item then
                        pageSize
                    else
                        0
            in
                { model
                    | comments =
                        Result.withDefault [] result
                            |> foldItems model.comments
                            |> updateCount id
                            |> updateLoading False id
                    , loading = False
                    , showCount = model.showCount + delta
                }
                    ! []

        RouteChange route ->
            case route of
                Router.ViewItem id ->
                    if RemoteData.isDone model.item && id == getId model.item then
                        model ! []
                    else
                        ( { model | item = Loading, loading = True, showCount = 0 }, fetchItem id )

                _ ->
                    model ! []


fetchItem : Int -> Cmd Msg
fetchItem =
    Api.send ReceiveItem << Api.requestItem


fetchComments : Int -> Item -> Cmd Msg
fetchComments count item =
    Maybe.withDefault [] item.kids
        |> List.drop count
        |> List.take pageSize
        |> Api.requestItems
        |> Api.send (ReceiveComments item.id)


fetchNestedComment : Int -> Model -> ( Model, Cmd Msg )
fetchNestedComment id model =
    let
        cmd =
            Dict.get id model.comments
                |> Maybe.map (\{ item, showCount } -> fetchComments showCount item)
                |> Maybe.withDefault Cmd.none

        comments =
            updateLoading True id model.comments
    in
        ( { model | comments = comments }, cmd )


fetchItemComment : Model -> ( Model, Cmd Msg )
fetchItemComment model =
    let
        cmd =
            model.item
                |> RemoteData.map (fetchComments model.showCount)
                |> RemoteData.withDefault Cmd.none
    in
        ( { model | loading = True }, cmd )


foldItems : Comments -> List Item -> Comments
foldItems =
    List.foldl (\item -> Dict.insert item.id <| Comment 0 False item)


getId : RemoteData Item -> Int
getId =
    RemoteData.withDefault -1 << RemoteData.map .id


updateComment : (Comment -> Comment) -> Int -> Comments -> Comments
updateComment f id =
    Dict.update id <| Maybe.map f


updateCount : Int -> Comments -> Comments
updateCount =
    updateComment (\comment -> { comment | showCount = comment.showCount + pageSize })


updateLoading : Bool -> Int -> Comments -> Comments
updateLoading loading =
    updateComment (\comment -> { comment | loading = loading })

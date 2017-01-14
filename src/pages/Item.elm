module Pages.Item exposing (Model, Msg(RouteChange), init, update, view, subscriptions)

import Api
import DateFormat
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Item exposing (Item)
import ItemEntry
import LoadText
import RemoteData exposing (RemoteData(..))
import Router exposing (Route)
import Util


-- MODEL


pageSize : Int
pageSize =
    10


type alias Comment =
    { collapsed : Bool
    , showCount : Int
    , loading : Bool
    , loadText : LoadText.Model
    , item : Item
    }


type alias Comments =
    Dict Int Comment


type alias Model =
    { comments : Comments
    , item : RemoteData Item
    , showCount : Int
    , loadText : LoadText.Model
    , loading : Bool
    }


init : Route -> ( Model, Cmd Msg )
init route =
    updateRoute route
        { comments = Dict.empty
        , item = NotRequested
        , loading = False
        , loadText = LoadText.init False
        , showCount = 0
        }



-- VIEW


view : Model -> Html Msg
view model =
    case model.item of
        Done item ->
            div []
                [ ItemEntry.view True [ ItemEntry.By, ItemEntry.Score ] item
                , viewCommentsContainer model item
                ]

        Loading ->
            LoadText.view model.loadText

        _ ->
            text "There doesn't seem to be anything here."


viewCommentsContainer : Model -> Item -> Html Msg
viewCommentsContainer { comments, loadText, loading, showCount } item =
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
                                ++ " Total)"
                        ]
                    , viewComments comments <| List.take showCount kids
                    , viewShowMore item.id delta loading loadText
                    ]

        Nothing ->
            Util.empty


viewComments : Comments -> List Int -> Html Msg
viewComments comments ids =
    let
        viewHelper id =
            Dict.get id comments
                |> Util.viewMaybe (viewComment comments)
    in
        div [ Attr.class "comment-level" ] <| List.map viewHelper ids


viewComment : Comments -> Comment -> Html Msg
viewComment comments { collapsed, showCount, item, loadText, loading } =
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
                    [ text " on "
                    , time [] [ text <| DateFormat.format item.time ]
                    ]
                ]
            , Util.viewMaybe Util.viewHtmlContent item.text
            , Util.viewIf (showCount > 0) <|
                viewHider collapsed item.id
            , Util.viewIf (not collapsed) <|
                div []
                    [ viewComments comments <| List.take showCount kids
                    , viewShowMore item.id delta loading loadText
                    ]
            ]


viewHider : Bool -> Int -> Html Msg
viewHider collapsed id =
    viewShowLink (ToggleHide id)
        [ text <|
            if collapsed then
                "Show Replies [+]"
            else
                "Hide Replies [-]"
        ]


viewShowMore : Int -> Int -> Bool -> LoadText.Model -> Html Msg
viewShowMore id count loading loadText =
    if loading then
        LoadText.view loadText
    else if count > 0 then
        viewShowLink (FetchComments id)
            [ text <| "▬ " ++ (toString count) ++ getReplyText count
            ]
    else
        Util.empty


viewShowLink : Msg -> List (Html Msg) -> Html Msg
viewShowLink msg =
    a [ Attr.class "show-more", Util.jsLink, onClick msg ]


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
    | ItemLoadTextMsg LoadText.Msg
    | CommentLoadTextMsg Int LoadText.Msg
    | ToggleHide Int


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
                ( { model
                    | item = RemoteData.fromResult item
                    , loading = True
                    , loadText = LoadText.toggle True model.loadText
                  }
                , cmd
                )

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
                    , loadText = LoadText.toggle False model.loadText
                    , showCount = model.showCount + delta
                }
                    ! []

        RouteChange route ->
            updateRoute route model

        ItemLoadTextMsg childMsg ->
            { model | loadText = LoadText.update childMsg model.loadText } ! []

        CommentLoadTextMsg id childMsg ->
            { model | comments = updateComentLoadText childMsg id model.comments } ! []

        ToggleHide id ->
            { model | comments = updateCollapsed id model.comments } ! []


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
    in
        ( { model | comments = updateLoading True id model.comments }, cmd )


fetchItemComment : Model -> ( Model, Cmd Msg )
fetchItemComment model =
    let
        cmd =
            model.item
                |> RemoteData.map (fetchComments model.showCount)
                |> RemoteData.withDefault Cmd.none
    in
        ( { model
            | loading = True
            , loadText = LoadText.toggle True model.loadText
          }
        , cmd
        )


foldItems : Comments -> List Item -> Comments
foldItems =
    List.foldl
        (\item ->
            Dict.insert item.id <|
                { collapsed = False
                , showCount = 0
                , loading = False
                , loadText = LoadText.init False
                , item = item
                }
        )


updateRoute : Route -> Model -> ( Model, Cmd Msg )
updateRoute route model =
    case route of
        Router.ViewItem id ->
            if RemoteData.isDone model.item && id == getId model.item then
                model ! []
            else
                ( { model
                    | comments = Dict.empty
                    , item = Loading
                    , loading = True
                    , showCount = 0
                    , loadText = LoadText.toggle True model.loadText
                  }
                , fetchItem id
                )

        _ ->
            model ! []


getId : RemoteData Item -> Int
getId =
    RemoteData.withDefault -1 << RemoteData.map .id


updateComment : (Comment -> Comment) -> Int -> Comments -> Comments
updateComment f id =
    Dict.update id <| Maybe.map f


updateCount : Int -> Comments -> Comments
updateCount =
    updateComment <|
        \comment -> { comment | showCount = comment.showCount + pageSize }


updateLoading : Bool -> Int -> Comments -> Comments
updateLoading loading =
    updateComment <|
        \comment ->
            { comment
                | loading = loading
                , loadText = LoadText.toggle loading comment.loadText
            }


updateCollapsed : Int -> Comments -> Comments
updateCollapsed =
    updateComment <|
        \comment -> { comment | collapsed = not comment.collapsed }


updateComentLoadText : LoadText.Msg -> Int -> Comments -> Comments
updateComentLoadText msg =
    updateComment <|
        \comment -> { comment | loadText = LoadText.update msg comment.loadText }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        mapSub ( id, { loadText } ) =
            Sub.map (CommentLoadTextMsg id) <| LoadText.subscriptions loadText
    in
        Sub.batch <|
            [ Sub.map ItemLoadTextMsg <| LoadText.subscriptions model.loadText ]
                ++ (List.map mapSub <| Dict.toList model.comments)

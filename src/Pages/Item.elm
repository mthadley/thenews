module Pages.Item exposing (Model, Msg, init, update, view, subscriptions)

import Dict exposing (Dict)
import Elements
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import PageTitle
import RemoteData exposing (RemoteData(..), WebData)
import Router exposing (Route)
import Store exposing (Action, Store)
import Types.Item as Item exposing (Item)
import Util.DateFormat as DateFormat
import Util.List exposing (zip, takeMaybe)
import Util.Html
    exposing
        ( empty
        , jsLink
        , pluralize
        , viewIf
        , viewMaybe
        , viewHtmlContent
        )
import Views.Item as ItemView
import Views.LoadText as LoadText


-- MODEL


type alias Comment =
    { collapsed : Bool
    , showCount : Int
    , loadText : LoadText.Model
    }


type alias Comments =
    Dict Int Comment


type alias Model =
    { comments : Comments
    , id : Int
    }


init : Store -> Int -> ( Model, Action Msg )
init store id =
    ( Model (initComments store id) id
    , Store.tag (RecieveItem id True) <| Store.requestItem id
    )


initComment : Comment
initComment =
    { collapsed = False
    , showCount = 0
    , loadText = LoadText.init
    }


initComments : Store -> Int -> Comments
initComments store id =
    let
        helper id comments =
            let
                kids =
                    Store.getItem store id
                        |> RemoteData.map (Maybe.withDefault [] << .kids)
                        |> RemoteData.withDefault []
                        |> List.filter (RemoteData.isSuccess << Store.getItem store)

                newComments =
                    Dict.insert
                        id
                        { initComment | showCount = List.length kids }
                        comments
            in
                List.foldl helper newComments kids
    in
        helper id Dict.empty



-- VIEW


view : Store -> Model -> Html Msg
view store model =
    case
        ( Store.getItem store model.id
        , Dict.get model.id model.comments
        )
    of
        ( Success item, Just comment ) ->
            div []
                [ ItemView.view [ ItemView.textContent, ItemView.by, ItemView.score ] item
                , viewCommentsContainer store model.comments item comment
                ]

        ( Loading, Just comment ) ->
            LoadText.view comment.loadText

        ( _, _ ) ->
            text "There doesn't seem to be anything here."


viewCommentsContainer : Store -> Comments -> Item -> Comment -> Html Msg
viewCommentsContainer store comments item { loadText, showCount } =
    viewMaybe
        (\kids ->
            let
                count =
                    List.length kids

                visibleKids =
                    List.take showCount kids

                loading =
                    RemoteData.isLoading <| Store.getItems store visibleKids
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
                    , viewComments store comments visibleKids
                    , viewShowMore item.id (count - showCount) loading loadText
                    ]
        )
        item.kids


viewComments : Store -> Comments -> List Int -> Html Msg
viewComments store comments ids =
    let
        helper ( comment, item ) =
            Maybe.map (viewComment store comments item) comment
    in
        takeMaybe (RemoteData.toMaybe << Store.getItem store) ids
            |> zip (getComments comments ids)
            |> List.filterMap helper
            |> Elements.commentLevel []


viewComment : Store -> Comments -> Item -> Comment -> Html Msg
viewComment store comments item { collapsed, showCount, loadText } =
    let
        kids =
            Maybe.withDefault [] item.kids

        visibleKids =
            List.take showCount kids

        loading =
            RemoteData.isLoading <| Store.getItems store visibleKids
    in
        Elements.comment []
            [ Elements.author []
                [ a [ Router.linkTo <| Router.ViewUser item.by ]
                    [ text item.by ]
                , small []
                    [ text " on "
                    , time [] [ text <| DateFormat.format item.time ]
                    ]
                ]
            , viewMaybe viewHtmlContent item.text
            , viewIf (showCount > 0 && not loading) <| viewHider collapsed item.id
            , viewIf (not collapsed) <|
                div []
                    [ viewComments store comments visibleKids
                    , viewShowMore item.id (List.length kids - showCount) loading loadText
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
            [ text <| "▬ " ++ (toString count) ++ getReplyText count ]
    else
        empty


viewShowLink : Msg -> List (Html Msg) -> Html Msg
viewShowLink msg =
    Elements.showMore [ jsLink, onClick msg ]


getCommentsTitle : Item.Type -> String
getCommentsTitle type_ =
    case type_ of
        Item.Comment ->
            "Replies"

        _ ->
            "Comments"


getReplyText : Int -> String
getReplyText =
    pluralize " Reply" " Replies"



-- UPDATE


type Msg
    = FetchComments Int
    | CommentLoadTextMsg Int LoadText.Msg
    | ToggleHide Int
    | RecieveItem Int Bool


update : Store -> Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update store msg model =
    case msg of
        CommentLoadTextMsg id childMsg ->
            ( { model | comments = updateComentLoadText childMsg id model.comments }
            , Cmd.none
            , Store.none
            )

        FetchComments id ->
            let
                ( comments, action ) =
                    requestComments (Store.getItem store id) model.comments
            in
                ( { model | comments = comments }, Cmd.none, action )

        ToggleHide id ->
            ( { model | comments = updateCollapsed id model.comments }, Cmd.none, Store.none )

        RecieveItem id top ->
            let
                item =
                    Store.getItem store id

                comments =
                    Dict.update id (Just << Maybe.withDefault initComment) model.comments

                ( newComments, action ) =
                    if top then
                        requestComments item comments
                    else
                        ( comments, Store.none )
            in
                ( { model | comments = newComments }
                , if top then
                    setTitle item
                  else
                    Cmd.none
                , action
                )


updateComment : (Comment -> Comment) -> Int -> Comments -> Comments
updateComment f id =
    Dict.update id <| Maybe.map f


updateCount : Int -> Comments -> Comments
updateCount =
    updateComment <|
        \comment -> { comment | showCount = comment.showCount + Store.pageSize }


updateCollapsed : Int -> Comments -> Comments
updateCollapsed =
    updateComment <|
        \comment -> { comment | collapsed = not comment.collapsed }


updateComentLoadText : LoadText.Msg -> Int -> Comments -> Comments
updateComentLoadText msg =
    updateComment <|
        \comment -> { comment | loadText = LoadText.update msg comment.loadText }


requestComments : WebData Item -> Comments -> ( Comments, Action Msg )
requestComments item comments =
    let
        id =
            RemoteData.withDefault 0 <| RemoteData.map .id item

        count =
            Dict.get id comments
                |> Maybe.map .showCount
                |> Maybe.withDefault 0
    in
        item
            |> RemoteData.map
                (.kids
                    >> Maybe.withDefault []
                    >> List.drop count
                    >> List.take Store.pageSize
                )
            |> RemoteData.withDefault []
            |> List.map
                (\id -> Store.tag (RecieveItem id False) <| Store.requestItem id)
            |> Store.batch
            |> (,) (updateCount id comments)


getComments : Comments -> List Int -> List (Maybe Comment)
getComments comments =
    List.foldr ((::) << (flip Dict.get) comments) []


getTitle : Item -> String
getTitle { id, title } =
    Maybe.withDefault (toString id) title


setTitle : WebData Item -> Cmd msg
setTitle item =
    item
        |> RemoteData.map (PageTitle.set << getTitle)
        |> RemoteData.withDefault Cmd.none


subscriptions : Store -> Model -> Sub Msg
subscriptions store model =
    let
        rootSub =
            Store.getItem store model.id
                |> RemoteData.isLoading
                |> mapSub model.id
    in
        Dict.toList model.comments
            |> List.map (commentSub store model)
            |> (::) rootSub
            |> Sub.batch


commentSub : Store -> Model -> ( Int, Comment ) -> Sub Msg
commentSub store model ( id, comment ) =
    Store.getItem store id
        |> RemoteData.map
            (List.take comment.showCount << Maybe.withDefault [] << .kids)
        |> RemoteData.withDefault []
        |> Store.getItems store
        |> RemoteData.isLoading
        |> mapSub id


mapSub : Int -> Bool -> Sub Msg
mapSub id =
    Sub.map (CommentLoadTextMsg id) << LoadText.subscriptions

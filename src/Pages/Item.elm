module Pages.Item exposing (Model, Msg, init, subscriptions, update, view)

import Css exposing (num, pct, px, zero)
import Data.Item as Item exposing (Item)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import Router exposing (Route)
import Store exposing (Action, Store)
import Tagged
import Tagged.Dict as Dict exposing (TaggedDict)
import Theme
import Util.DateFormat as DateFormat
import Util.Html
    exposing
        ( empty
        , pluralize
        , viewHtmlContent
        , viewIf
        , viewMaybe
        )
import Util.List
import Views.Item as ItemView
import Views.LoadText as LoadText



-- MODEL


type alias Comment =
    { collapsed : Bool
    , showCount : Int
    , loadText : LoadText.Model
    }


type alias Comments =
    TaggedDict Item.Ident Int Comment


type alias Model =
    { comments : Comments
    , id : Item.Id
    }


init : Store -> Item.Id -> ( Model, Action Msg )
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


initComments : Store -> Item.Id -> Comments
initComments store postId =
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
    helper postId Dict.empty



-- VIEW


view : Store -> Model -> ( String, Html Msg )
view store model =
    case
        ( Store.getItem store model.id
        , Dict.get model.id model.comments
        )
    of
        ( Success item, Just comment ) ->
            ( getTitle item
            , div []
                [ ItemView.view
                    [ ItemView.textContent ExternalLink
                    , ItemView.by
                    , ItemView.score
                    ]
                    item
                , viewCommentsContainer store model.comments item comment
                ]
            )

        ( Loading, Just comment ) ->
            ( LoadText.viewString comment.loadText, LoadText.view comment.loadText )

        ( _, _ ) ->
            ( "Nothing...", text "There doesn't seem to be anything here." )


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
                        getCommentsTitle item.type_
                            ++ " ("
                            ++ String.fromInt count
                            ++ getReplyText count
                            ++ ", "
                            ++ (String.fromInt <| Maybe.withDefault 0 item.descendants)
                            ++ " Total)"
                    ]
                , viewComments store comments visibleKids
                , viewShowMore item.id (count - showCount) loading loadText
                ]
        )
        item.kids


viewComments : Store -> Comments -> List Item.Id -> Html Msg
viewComments store comments ids =
    let
        helper ( comment, item ) =
            Maybe.map (viewComment store comments item) comment

        theme =
            Store.getTheme store
    in
    Util.List.takeMap (RemoteData.toMaybe << Store.getItem store) ids
        |> List.Extra.zip (getComments comments ids)
        |> List.filterMap helper
        |> styled div
            [ Css.marginLeft (px Theme.commentLevelMargin)
            , Css.position Css.relative
            , Css.after
                [ Css.backgroundColor (Theme.colors theme).primary
                , Theme.termShadow theme
                , Css.property "content" "''"
                , Css.height <| pct 100
                , Css.left <| px (-1 * Theme.commentLevelMargin)
                , Css.opacity <| num 0.8
                , Css.position Css.absolute
                , Css.top zero
                , Css.width <| px 2
                ]
            ]
            []


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
    styled article
        [ Css.fontSize Theme.fontSizes.comment
        , Css.marginBottom <| px 40
        ]
        []
        [ styled h3
            [ Css.fontStyle Css.italic ]
            []
            [ a [ Router.linkTo <| Router.ViewUser item.by ]
                [ text <| Tagged.untag item.by ]
            , small []
                [ text " on "
                , time []
                    [ a [ Router.linkTo <| Router.ViewItem item.id ]
                        [ text <| DateFormat.format (Store.getZone store) item.time ]
                    ]
                ]
            ]
        , viewMaybe (viewHtmlContent ExternalLink) item.text
        , viewIf (\() -> viewHider collapsed item.id)
            (showCount > 0 && not loading)
        , viewIf
            (\() ->
                div []
                    [ viewComments store comments visibleKids
                    , viewShowMore item.id (List.length kids - showCount) loading loadText
                    ]
            )
            (not collapsed)
        ]


viewHider : Bool -> Item.Id -> Html Msg
viewHider collapsed id =
    viewShowLink (ToggleHide id)
        [ text <|
            if collapsed then
                "Show Replies [+]"

            else
                "Hide Replies [-]"
        ]


viewShowMore : Item.Id -> Int -> Bool -> LoadText.Model -> Html Msg
viewShowMore id count loading loadText =
    if loading then
        LoadText.view loadText

    else if count > 0 then
        viewShowLink (FetchComments id)
            [ text <| "▬ " ++ String.fromInt count ++ getReplyText count ]

    else
        empty


viewShowLink : Msg -> List (Html Msg) -> Html Msg
viewShowLink msg =
    styled button
        [ Css.backgroundColor Css.transparent
        , Css.borderWidth Css.zero
        , Css.fontWeight Css.bold
        , Css.color Css.inherit
        , Css.fontSize Css.inherit
        , Css.fontFamily Css.inherit
        , Css.textShadow Css.inherit
        , Css.padding Css.zero
        ]
        [ onClick msg ]


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
    = FetchComments Item.Id
    | CommentLoadTextMsg Item.Id LoadText.Msg
    | ToggleHide Item.Id
    | RecieveItem Item.Id Bool
    | ExternalLink String


update : Store -> Msg -> Model -> ( Model, Action Msg )
update store msg model =
    case msg of
        CommentLoadTextMsg id childMsg ->
            ( { model | comments = updateComentLoadText childMsg id model.comments }
            , Store.none
            )

        FetchComments id ->
            let
                ( comments, action ) =
                    requestComments (Store.getItem store id) model.comments
            in
            ( { model | comments = comments }, action )

        ToggleHide id ->
            ( { model | comments = updateCollapsed id model.comments }, Store.none )

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
            , action
            )

        ExternalLink href ->
            ( model, Store.navigate href )


updateComment : (Comment -> Comment) -> Item.Id -> Comments -> Comments
updateComment f id =
    Dict.update id <| Maybe.map f


updateCount : Item.Id -> Comments -> Comments
updateCount =
    updateComment <|
        \comment -> { comment | showCount = comment.showCount + Store.pageSize }


updateCollapsed : Item.Id -> Comments -> Comments
updateCollapsed =
    updateComment <|
        \comment -> { comment | collapsed = not comment.collapsed }


updateComentLoadText : LoadText.Msg -> Item.Id -> Comments -> Comments
updateComentLoadText msg =
    updateComment <|
        \comment -> { comment | loadText = LoadText.update msg comment.loadText }


requestComments : WebData Item -> Comments -> ( Comments, Action Msg )
requestComments item comments =
    let
        postId =
            item
                |> RemoteData.map .id
                |> RemoteData.withDefault (Tagged.tag 0)

        count =
            Dict.get postId comments
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
        |> Tuple.pair (updateCount postId comments)


getComments : Comments -> List Item.Id -> List (Maybe Comment)
getComments comments =
    List.foldr ((::) << (\id -> Dict.get id comments)) []


getTitle : Item -> String
getTitle { id, title } =
    Maybe.withDefault (String.fromInt <| Tagged.untag id) title


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


commentSub : Store -> Model -> ( Item.Id, Comment ) -> Sub Msg
commentSub store model ( id, comment ) =
    Store.getItem store id
        |> RemoteData.map
            (List.take comment.showCount << Maybe.withDefault [] << .kids)
        |> RemoteData.withDefault []
        |> Store.getItems store
        |> RemoteData.isLoading
        |> mapSub id


mapSub : Item.Id -> Bool -> Sub Msg
mapSub id =
    Sub.map (CommentLoadTextMsg id) << LoadText.subscriptions

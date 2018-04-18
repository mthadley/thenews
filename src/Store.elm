module Store
    exposing
        ( Action
        , Store
        , batch
        , getCategory
        , getItem
        , getItems
        , getUser
        , init
        , map
        , none
        , pageSize
        , requestCategory
        , requestItem
        , requestUser
        , tag
        , update
        )

import Api exposing (Category)
import Dict exposing (Dict)
import RemoteData exposing (RemoteData(..), WebData)
import Tagged exposing (Tagged)
import Tagged.Dict exposing (TaggedDict)
import Task
import Types.Item as Item exposing (Item)
import Types.User as User exposing (User)
import Util.Tuple exposing (mapSecond, mapThird)


type alias Maps =
    { categories : Dict String (WebData (List Item.Id))
    , items : TaggedDict Item.Ident Int (WebData Item)
    , users : TaggedDict User.Ident String (WebData User)
    }


{-| A global store for cached data.
-}
type Store
    = Store Maps


init : Store
init =
    Store
        { categories = Dict.empty
        , users = Tagged.Dict.empty
        , items = Tagged.Dict.empty
        }


type Action msg
    = None
    | Batch (List (Action msg))
    | Tagged msg (Action msg)
    | TaggedResult msg (Action msg)
    | RecieveItem Item.Id (WebData Item)
    | RecieveUser User.Id (WebData User)
    | RequestItem Item.Id
    | RequestUser User.Id
    | RequestCategory Category
    | RecieveCategory Category (WebData (List Item.Id))


update : Action msg -> Store -> ( Store, Cmd (Action msg), Cmd msg )
update action ((Store maps) as store) =
    case action of
        None ->
            noop store

        Tagged msg action ->
            update action store
                |> mapSecond (Cmd.map <| TaggedResult msg)

        TaggedResult msg action ->
            let
                ( newStore, cmd, outCmd ) =
                    update action store
            in
            ( newStore
            , cmd
            , Cmd.batch
                [ Task.perform identity <| Task.succeed msg
                , outCmd
                ]
            )

        Batch actions ->
            let
                applyActions action ( store, cmd, outCmd ) =
                    update action store
                        |> mapSecond (\c -> Cmd.batch [ cmd, c ])
                        |> mapThird (\c -> Cmd.batch [ outCmd, c ])
            in
            List.foldl applyActions (noop store) actions

        RequestUser id ->
            ( Store
                { maps
                    | users = Tagged.Dict.update id setLoading maps.users
                }
            , Api.send (RecieveUser id) <| Api.requestUser <| Tagged.untag id
            , Cmd.none
            )

        RecieveUser id user ->
            noop <| Store { maps | users = Tagged.Dict.insert id user maps.users }

        RequestItem id ->
            ( Store
                { maps
                    | items = Tagged.Dict.update id setLoading maps.items
                }
            , fetchItem id
            , Cmd.none
            )

        RecieveItem id data ->
            noop <| Store { maps | items = Tagged.Dict.insert id data maps.items }

        RequestCategory category ->
            ( Store
                { maps
                    | categories =
                        Dict.update (Api.label category) setLoading maps.categories
                }
            , Api.send (RecieveCategory category) <| Api.requestCategoryIds category
            , Cmd.none
            )

        RecieveCategory category rawIds ->
            Store
                { maps
                    | categories =
                        Dict.insert
                            (Api.label category)
                            (RemoteData.map (List.take pageSize) rawIds)
                            maps.categories
                }
                |> noop


none : Action msg
none =
    None


batch : List (Action msg) -> Action msg
batch =
    Batch


map : (a -> msg) -> Action a -> Action msg
map f action =
    case action of
        None ->
            None

        Tagged msg action ->
            Tagged (f msg) <| map f action

        TaggedResult msg action ->
            TaggedResult (f msg) <| map f action

        Batch actions ->
            Batch <| List.map (map f) actions

        RequestItem id ->
            RequestItem id

        RecieveItem id data ->
            RecieveItem id data

        RequestUser id ->
            RequestUser id

        RecieveUser id data ->
            RecieveUser id data

        RequestCategory category ->
            RequestCategory category

        RecieveCategory category data ->
            RecieveCategory category data


tag : msg -> Action msg -> Action msg
tag =
    Tagged


requestCategory : Category -> Action msg
requestCategory =
    RequestCategory


requestUser : User.Id -> Action msg
requestUser =
    RequestUser


requestItem : Item.Id -> Action msg
requestItem =
    RequestItem


getCategory : Store -> Category -> WebData (List Item.Id)
getCategory (Store store) category =
    store.categories
        |> Dict.get (Api.label category)
        |> Maybe.withDefault NotAsked


getUser : Store -> User.Id -> WebData User
getUser =
    get .users


getItem : Store -> Item.Id -> WebData Item
getItem =
    get .items


getItems : Store -> List Item.Id -> WebData (List Item)
getItems store ids =
    RemoteData.fromList <| List.map (getItem store) ids


pageSize : Int
pageSize =
    10



-- Helpers


setLoading : Maybe (WebData a) -> Maybe (WebData a)
setLoading item =
    case Maybe.withDefault NotAsked item of
        Success a ->
            Just <| Success a

        _ ->
            Just Loading


fetchItem : Item.Id -> Cmd (Action msg)
fetchItem id =
    Api.send (RecieveItem id) <| Api.requestItem <| Tagged.untag id


noop : a -> ( a, Cmd b, Cmd c )
noop a =
    ( a, Cmd.none, Cmd.none )


get :
    (Maps -> TaggedDict k comparable (WebData a))
    -> Store
    -> Tagged k comparable
    -> WebData a
get f (Store store) id =
    Maybe.withDefault NotAsked <| Tagged.Dict.get id <| f store

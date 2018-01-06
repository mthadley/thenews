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
import Task
import Types.Item exposing (Item)
import Types.User exposing (User)
import Util.Tuple exposing (mapSecond, mapThird)


type alias Maps =
    { categories : Dict String (WebData (List Int))
    , items : Dict Int (WebData Item)
    , users : Dict String (WebData User)
    }


{-| A global store for cached data.
-}
type Store
    = Store Maps


init : Store
init =
    Store
        { categories = Dict.empty
        , users = Dict.empty
        , items = Dict.empty
        }


type Action msg
    = None
    | Batch (List (Action msg))
    | Tagged msg (Action msg)
    | TaggedResult msg (Action msg)
    | RecieveItem Int (WebData Item)
    | RecieveUser String (WebData User)
    | RequestItem Int
    | RequestUser String
    | RequestCategory Category
    | RecieveCategory Category (WebData (List Int))


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
                    | users = Dict.update id setLoading maps.users
                }
            , Api.send (RecieveUser id) <| Api.requestUser id
            , Cmd.none
            )

        RecieveUser id user ->
            noop <| Store { maps | users = Dict.insert id user maps.users }

        RequestItem id ->
            ( Store
                { maps
                    | items = Dict.update id setLoading maps.items
                }
            , fetchItem id
            , Cmd.none
            )

        RecieveItem id data ->
            noop <| Store { maps | items = Dict.insert id data maps.items }

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


requestUser : String -> Action msg
requestUser =
    RequestUser


requestItem : Int -> Action msg
requestItem =
    RequestItem


getCategory : Store -> Category -> WebData (List Int)
getCategory store category =
    get .categories store <| Api.label category


getUser : Store -> String -> WebData User
getUser =
    get .users


getItem : Store -> Int -> WebData Item
getItem =
    get .items


getItems : Store -> List Int -> WebData (List Item)
getItems store ids =
    fromList <| List.map (getItem store) ids


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


fetchItem : Int -> Cmd (Action msg)
fetchItem id =
    Api.send (RecieveItem id) <| Api.requestItem id


fromList : List (WebData a) -> WebData (List a)
fromList =
    List.foldr (RemoteData.map2 (::)) (Success [])


noop : a -> ( a, Cmd b, Cmd c )
noop a =
    ( a, Cmd.none, Cmd.none )


get :
    (Maps -> Dict comparable (WebData a))
    -> Store
    -> comparable
    -> WebData a
get f (Store store) id =
    Maybe.withDefault NotAsked <| Dict.get id <| f store

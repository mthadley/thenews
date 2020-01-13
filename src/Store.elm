module Store exposing
    ( Action
    , Store
    , batch
    , getCategory
    , getItem
    , getItems
    , getTheme
    , getThemePreference
    , getUser
    , getZone
    , init
    , map
    , navigate
    , none
    , pageSize
    , requestCategory
    , requestItem
    , requestUser
    , setThemePreference
    , subscriptions
    , tag
    , update
    )

import Api
import Browser.Navigation as Navigation exposing (Key)
import Data.Category as Category exposing (Category)
import Data.Item as Item exposing (Item)
import Data.User as User exposing (User)
import RemoteData exposing (RemoteData(..), WebData)
import Router
import Sort.Dict
import Tagged exposing (Tagged)
import Tagged.Dict exposing (TaggedDict)
import Task
import Theme exposing (Theme)
import Theme.Preference as ThemePreference
import Time
import Util.Tuple exposing (mapSecond, mapThird)


type alias Info =
    { categories : Sort.Dict.Dict Category (WebData (List Item.Id))
    , items : TaggedDict Item.Ident Int (WebData Item)
    , users : TaggedDict User.Ident String (WebData User)
    , zone : Time.Zone
    , themePreference : ThemePreference.Preference
    }


{-| A global store for cached data.
-}
type Store
    = Store Info


init : ThemePreference.Preference -> ( Store, Cmd (Action msg) )
init themePreference =
    ( Store
        { categories = Sort.Dict.empty Category.sorter
        , users = Tagged.Dict.empty
        , items = Tagged.Dict.empty
        , zone = Time.utc
        , themePreference = themePreference
        }
    , Task.perform RecieveTimeZone Time.here
    )


type Action msg
    = None
    | Batch (List (Action msg))
    | Tagged msg (Action msg)
    | TaggedResult msg (Action msg)
    | Navigate String
    | RecieveItem Item.Id (WebData Item)
    | RecieveUser User.Id (WebData User)
    | RequestItem Item.Id
    | RequestUser User.Id
    | RequestCategory Category
    | RecieveCategory Category (WebData (List Item.Id))
    | RecieveTimeZone Time.Zone
    | SetThemePreference ThemePreference.Selection
    | RecieveThemePreference ThemePreference.Preference


update : Key -> Action msg -> Store -> ( Store, Cmd (Action msg), Cmd msg )
update key action ((Store info) as store) =
    case action of
        None ->
            noop store

        Tagged msg action_ ->
            update key action_ store
                |> mapSecond (Cmd.map <| TaggedResult msg)

        TaggedResult msg action_ ->
            let
                ( newStore, cmd, outCmd ) =
                    update key action_ store
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
                applyActions action_ ( store_, cmd, outCmd ) =
                    update key action_ store_
                        |> mapSecond (\c -> Cmd.batch [ cmd, c ])
                        |> mapThird (\c -> Cmd.batch [ outCmd, c ])
            in
            List.foldl applyActions (noop store) actions

        Navigate url ->
            ( store, Cmd.none, Router.redirectExternal key url )

        RequestUser id ->
            ( Store
                { info
                    | users = Tagged.Dict.update id setLoading info.users
                }
            , Api.send (RecieveUser id) <| Api.requestUser <| Tagged.untag id
            , Cmd.none
            )

        RecieveUser id user ->
            noop <| Store { info | users = Tagged.Dict.insert id user info.users }

        RequestItem id ->
            ( Store
                { info
                    | items = Tagged.Dict.update id setLoading info.items
                }
            , fetchItem id
            , Cmd.none
            )

        RecieveItem id data ->
            noop <| Store { info | items = Tagged.Dict.insert id data info.items }

        RequestCategory category ->
            ( Store
                { info
                    | categories =
                        Sort.Dict.update category setLoading info.categories
                }
            , Api.send (RecieveCategory category) <| Api.requestCategoryIds category
            , Cmd.none
            )

        RecieveCategory category rawIds ->
            Store
                { info
                    | categories =
                        Sort.Dict.insert category
                            (RemoteData.map (List.take pageSize) rawIds)
                            info.categories
                }
                |> noop

        RecieveTimeZone zone ->
            Store { info | zone = zone } |> noop

        SetThemePreference selection ->
            let
                ( newThemePreference, cmd ) =
                    ThemePreference.select selection info.themePreference
            in
            ( Store { info | themePreference = newThemePreference }, cmd, Cmd.none )

        RecieveThemePreference themePreference ->
            Store { info | themePreference = themePreference } |> noop


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

        Tagged msg taggedAction ->
            Tagged (f msg) <| map f taggedAction

        TaggedResult msg taggedAction ->
            TaggedResult (f msg) <| map f taggedAction

        Batch actions ->
            Batch <| List.map (map f) actions

        Navigate url ->
            Navigate url

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

        RecieveTimeZone zone ->
            RecieveTimeZone zone

        SetThemePreference selection ->
            SetThemePreference selection

        RecieveThemePreference themePreference ->
            RecieveThemePreference themePreference


tag : msg -> Action msg -> Action msg
tag =
    Tagged


navigate : String -> Action msg
navigate =
    Navigate


requestCategory : Category -> Action msg
requestCategory =
    RequestCategory


requestUser : User.Id -> Action msg
requestUser =
    RequestUser


requestItem : Item.Id -> Action msg
requestItem =
    RequestItem


setThemePreference : ThemePreference.Selection -> Action msg
setThemePreference =
    SetThemePreference


getCategory : Store -> Category -> WebData (List Item.Id)
getCategory (Store store) category =
    store.categories
        |> Sort.Dict.get category
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


getZone : Store -> Time.Zone
getZone (Store { zone }) =
    zone


getTheme : Store -> Theme
getTheme =
    ThemePreference.theme << getThemePreference


getThemePreference : Store -> ThemePreference.Preference
getThemePreference (Store { themePreference }) =
    themePreference


subscriptions : Store -> Sub (Action msg)
subscriptions (Store { themePreference }) =
    Sub.map RecieveThemePreference (ThemePreference.subscriptions themePreference)


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
    (Info -> TaggedDict k comparable (WebData a))
    -> Store
    -> Tagged k comparable
    -> WebData a
get f (Store store) id =
    Maybe.withDefault NotAsked <| Tagged.Dict.get id <| f store

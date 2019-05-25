module Api exposing
    ( Category(..)
    , Task
    , base
    , categoryEndpoint
    , label
    , requestCategoryIds
    , requestItem
    , requestItems
    , requestUrl
    , requestUser
    , send
    , stringId
    )

import Http exposing (Request)
import Json.Decode as Decode
import RemoteData exposing (WebData)
import Task exposing (Task)
import Types.Item as Item exposing (Item)
import Types.User as User exposing (User)
import Util.Json exposing (tag)


type alias Task a =
    Task.Task Http.Error a


type Category
    = Ask
    | Best
    | Job
    | New
    | Show
    | Top


base : String
base =
    "https://hacker-news.firebaseio.com/v0/"


categoryEndpoint : Category -> String
categoryEndpoint category =
    case category of
        Ask ->
            "askstories"

        Best ->
            "beststories"

        Job ->
            "jobstories"

        New ->
            "newstories"

        Show ->
            "showstories"

        Top ->
            "topstories"


requestCategoryIds : Category -> Task (List Item.Id)
requestCategoryIds category =
    Decode.list (tag Decode.int)
        |> Http.get (requestUrl <| categoryEndpoint category)
        |> Http.toTask


requestItems : List Int -> Task (List Item)
requestItems =
    Task.sequence << List.map requestItem


requestItem : Int -> Task Item
requestItem id =
    Http.toTask <|
        Http.get (requestUrl <| "item/" ++ String.fromInt id) Item.decode


requestUrl : String -> String
requestUrl endpoint =
    base ++ endpoint ++ ".json"


requestUser : String -> Task User
requestUser id =
    Http.toTask <|
        Http.get (requestUrl <| "user/" ++ id) User.decode


send : (WebData a -> msg) -> Task a -> Cmd msg
send msg =
    Task.attempt (msg << RemoteData.fromResult)


stringId : Category -> String
stringId =
    categoryEndpoint


label : Category -> String
label category =
    case category of
        Ask ->
            "Ask"

        Best ->
            "Best"

        Job ->
            "Job"

        New ->
            "New"

        Show ->
            "Show"

        Top ->
            "Top"

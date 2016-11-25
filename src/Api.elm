module Api exposing (..)

import Http exposing (Request)
import Item exposing (Item)
import Json.Decode as Decode
import Task exposing (Task)


type RemoteData a
    = Loading
    | Done a
    | Error Http.Error
    | NotRequested


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


requestCategoryIds : Category -> Task Http.Error (List Int)
requestCategoryIds category =
    Http.toTask <|
        Http.get (requestUrl <| categoryEndpoint category) <|
            Decode.list Decode.int


requestCategory : Category -> Task Http.Error (List Item)
requestCategory category =
    requestCategoryIds category
        |> Task.andThen (requestItems << List.take 10)


requestItems : List Int -> Task Http.Error (List Item)
requestItems =
    Task.sequence << List.map (Http.toTask << requestItem)


requestItem : Int -> Request Item
requestItem id =
    Http.get (requestUrl <| "item/" ++ (toString id)) Item.decode


requestUrl : String -> String
requestUrl endpoint =
    base ++ endpoint ++ ".json"


resultToRemoteData : Result Http.Error a -> RemoteData a
resultToRemoteData result =
    case result of
        Ok a ->
            Done a

        Err err ->
            Error err


send : (RemoteData a -> msg) -> Task Http.Error a -> Cmd msg
send msg =
    Task.attempt <| msg << resultToRemoteData


stringId : Category -> String
stringId =
    categoryEndpoint

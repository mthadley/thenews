module Api exposing (..)

import Http exposing (Request)
import Item exposing (Item)
import Json.Decode as Decode
import Result
import Task exposing (Task)
import User exposing (User)


type alias Result a =
    Result.Result Http.Error a


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


requestCategoryIds : Category -> Task (List Int)
requestCategoryIds category =
    Decode.list Decode.int
        |> Http.get (requestUrl <| categoryEndpoint category)
        |> Http.toTask


requestCategory : Category -> Task (List Item)
requestCategory category =
    requestCategoryIds category
        |> Task.andThen (requestItems << List.take 10)


requestItems : List Int -> Task (List Item)
requestItems =
    Task.sequence << List.map requestItem


requestItem : Int -> Task Item
requestItem id =
    Http.toTask <|
        Http.get (requestUrl <| "item/" ++ (toString id)) Item.decode


requestUrl : String -> String
requestUrl endpoint =
    base ++ endpoint ++ ".json"


requestUser : String -> Task User
requestUser id =
    Http.toTask <|
        Http.get (requestUrl <| "user/" ++ id) User.decode


send : (Result a -> msg) -> Task a -> Cmd msg
send =
    Task.attempt


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

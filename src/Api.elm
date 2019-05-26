module Api exposing
    ( Task
    , base
    , categoryEndpoint
    , requestCategoryIds
    , requestItem
    , requestItems
    , requestUrl
    , requestUser
    , send
    )

import Data.Category as Category exposing (Category(..))
import Data.Item as Item exposing (Item)
import Data.User as User exposing (User)
import Http
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (WebData)
import Task exposing (Task)
import Util.Json exposing (tag)


type alias Task a =
    Task.Task Http.Error a


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
    get (requestUrl <| categoryEndpoint category)
        (Decode.list <| tag Decode.int)


requestItems : List Int -> Task (List Item)
requestItems =
    Task.sequence << List.map requestItem


requestItem : Int -> Task Item
requestItem id =
    get (requestUrl <| "item/" ++ String.fromInt id) Item.decode


requestUser : String -> Task User
requestUser id =
    get (requestUrl <| "user/" ++ id) User.decode


send : (WebData a -> msg) -> Task a -> Cmd msg
send msg =
    Task.attempt (msg << RemoteData.fromResult)


requestUrl : String -> String
requestUrl endpoint =
    base ++ endpoint ++ ".json"


get : String -> Decoder a -> Task a
get url decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver =
            Http.stringResolver <|
                \response ->
                    case response of
                        Http.BadUrl_ url_ ->
                            Err (Http.BadUrl url_)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata body ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ metadata body ->
                            case Decode.decodeString decoder body of
                                Ok value ->
                                    Ok value

                                Err err ->
                                    Err (Http.BadBody (Decode.errorToString err))
        }

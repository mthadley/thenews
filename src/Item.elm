module Item exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (required, optional)
import Util exposing (optionalMaybe)


type Type
    = Job
    | Story
    | Comment
    | Poll
    | PollOption


type alias Item =
    { by : String
    , dead : Bool
    , deleted : Bool
    , descendants : Maybe Int
    , id : Int
    , kids : Maybe (List Int)
    , parent : Maybe Int
    , parts : Maybe (List PollOpt)
    , score : Maybe Int
    , text : Maybe String
    , time : Int
    , title : Maybe String
    , type_ : Type
    , url : Maybe String
    }


type alias PollOpt =
    { by : String
    , id : Int
    , parent : Int
    , score : Int
    , text : String
    , time : Int
    }


decode : Decoder Item
decode =
    Pipeline.decode Item
        |> optional "by" string "Deleted"
        |> optional "dead" bool False
        |> optional "deleted" bool False
        |> optionalMaybe "descendants" int
        |> required "id" int
        |> optionalMaybe "kids" (list int)
        |> optionalMaybe "parent" int
        |> optionalMaybe "parts" (list pollOpt)
        |> optionalMaybe "score" int
        |> optionalMaybe "text" string
        |> required "time" int
        |> optionalMaybe "title" string
        |> required "type" (Json.Decode.map decodeType string)
        |> optionalMaybe "url" string


decodeType : String -> Type
decodeType str =
    case str of
        "job" ->
            Job

        "story" ->
            Story

        "comment" ->
            Comment

        "poll" ->
            Poll

        "pollopt" ->
            PollOption

        _ ->
            Story


pollOpt : Decoder PollOpt
pollOpt =
    Pipeline.decode PollOpt
        |> required "by" string
        |> required "int" int
        |> required "parent" int
        |> required "score" int
        |> required "text" string
        |> required "time" int

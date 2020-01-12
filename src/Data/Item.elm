module Data.Item exposing
    ( Id
    , Ident
    , Item
    , PollOpt
    , Type(..)
    , decode
    , decodeType
    , pollOpt
    )

import Data.Item.Id
import Data.User as User
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Router
import Tagged exposing (Tagged)
import Util.Json exposing (optionalMaybe, tag)


type alias Ident =
    Data.Item.Id.Ident


type alias Id =
    Data.Item.Id.Id


type Type
    = Job
    | Story
    | Comment
    | Poll
    | PollOption


type alias Item =
    { id : Id
    , by : User.Id
    , dead : Bool
    , deleted : Bool
    , descendants : Maybe Int
    , kids : Maybe (List Id)
    , parent : Maybe Id
    , parts : Maybe (List PollOpt)
    , score : Maybe Int
    , text : Maybe String
    , time : Int
    , title : Maybe String
    , type_ : Type
    , url : String
    }


type alias PollOpt =
    { by : User.Id
    , id : Int
    , parent : Id
    , score : Int
    , text : String
    , time : Int
    }


decode : Decoder Item
decode =
    field "id" (tag Decode.int)
        |> Decode.andThen
            (\id ->
                Decode.succeed (Item id)
                    |> optional "by" (tag Decode.string) (Tagged.tag "Deleted")
                    |> optional "dead" Decode.bool False
                    |> optional "deleted" Decode.bool False
                    |> optionalMaybe "descendants" Decode.int
                    |> optionalMaybe "kids" (Decode.list <| tag Decode.int)
                    |> optionalMaybe "parent" (tag Decode.int)
                    |> optionalMaybe "parts" (Decode.list pollOpt)
                    |> optionalMaybe "score" Decode.int
                    |> optionalMaybe "text" Decode.string
                    |> required "time" Decode.int
                    |> optionalMaybe "title" Decode.string
                    |> required "type" (Decode.map decodeType Decode.string)
                    |> optional "url" Decode.string (defaultItemUrl id)
            )


defaultItemUrl : Id -> String
defaultItemUrl =
    Router.reverse << Router.ViewItem


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
    Decode.succeed PollOpt
        |> required "by" (tag Decode.string)
        |> required "int" Decode.int
        |> required "parent" (tag Decode.int)
        |> required "score" Decode.int
        |> required "text" Decode.string
        |> required "time" Decode.int

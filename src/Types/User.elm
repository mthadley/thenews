module Types.User exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Util.Json exposing (optionalMaybe)


type alias User =
    { about : Maybe String
    , created : Int
    , delay : Int
    , id : String
    , karma : Int
    , submitted : List Int
    }


decode : Decoder User
decode =
    Pipeline.decode User
        |> optionalMaybe "about" string
        |> required "created" int
        |> optional "delay" int 0
        |> required "id" string
        |> required "karma" int
        |> required "submitted" (list int)

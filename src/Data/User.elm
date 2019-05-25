module Data.User exposing (Id, Ident, User, decode)

import Data.Item.Id as Item
import Data.User.Id
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Util.Json exposing (optionalMaybe, tag)


type alias Id =
    Data.User.Id.Id


type alias Ident =
    Data.User.Id.Ident


type alias User =
    { about : Maybe String
    , created : Int
    , delay : Int
    , id : Id
    , karma : Int
    , submitted : List Item.Id
    }


decode : Decoder User
decode =
    Decode.succeed User
        |> optionalMaybe "about" Decode.string
        |> required "created" Decode.int
        |> optional "delay" Decode.int 0
        |> required "id" (tag Decode.string)
        |> required "karma" Decode.int
        |> required "submitted" (Decode.list <| tag Decode.int)

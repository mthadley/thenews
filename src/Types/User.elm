module Types.User exposing (Id, Ident, User, decode)

import Json.Decode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Types.Item.Id as Item
import Types.User.Id
import Util.Json exposing (optionalMaybe, tag)


type alias Id =
    Types.User.Id.Id


type alias Ident =
    Types.User.Id.Ident


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
    Pipeline.decode User
        |> optionalMaybe "about" string
        |> required "created" int
        |> optional "delay" int 0
        |> required "id" (tag string)
        |> required "karma" int
        |> required "submitted" (list <| tag int)

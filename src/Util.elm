module Util exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)


optionalMaybe : String -> Decoder a -> (Decoder (Maybe a -> b) -> Decoder b)
optionalMaybe field decoder =
    optional field (Decode.map Just decoder) Nothing


detailString : List ( String, Maybe String ) -> String
detailString =
    let
        detail ( name, value ) =
            Maybe.map ((++) name) value
    in
        String.join " • " << List.filterMap detail


maybeToString : Maybe a -> Maybe String
maybeToString =
    Maybe.map toString

module Util.Json exposing (maybeToString, optionalMaybe)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)


maybeToString : Maybe a -> Maybe String
maybeToString =
    Maybe.map toString


optionalMaybe : String -> Decoder a -> (Decoder (Maybe a -> b) -> Decoder b)
optionalMaybe field decoder =
    optional field (Decode.map Just decoder) Nothing

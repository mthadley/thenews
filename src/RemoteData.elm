module RemoteData exposing (..)

import Http


type RemoteData a
    = Loading
    | Done a
    | Error Http.Error
    | NotRequested


fromResult : Result Http.Error a -> RemoteData a
fromResult result =
    case result of
        Ok a ->
            Done a

        Err err ->
            Error err


isDone : RemoteData a -> Bool
isDone data =
    case data of
        Done _ ->
            True

        _ ->
            False

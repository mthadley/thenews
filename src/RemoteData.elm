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


isLoading : RemoteData a -> Bool
isLoading data =
    case data of
        Loading ->
            True

        _ ->
            False


withDefault : a -> RemoteData a -> a
withDefault default result =
    case result of
        Done data ->
            data

        _ ->
            default


map : (a -> b) -> RemoteData a -> RemoteData b
map f remoteData =
    case remoteData of
        Done data ->
            Done <| f data

        Loading ->
            Loading

        NotRequested ->
            NotRequested

        Error err ->
            Error err

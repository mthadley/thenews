module Util.DateFormat exposing (format)

import Time exposing (Month(..), Zone)


format : Zone -> Int -> String
format zone =
    formatDate zone << Time.millisToPosix << (*) 1000


formatDate : Zone -> Time.Posix -> String
formatDate zone posix =
    formatMonth zone posix
        ++ " "
        ++ (String.fromInt <| Time.toDay zone posix)
        ++ ", "
        ++ (String.fromInt <| Time.toYear zone posix)
        ++ " at "
        ++ (paddedString <| Time.toHour zone posix)
        ++ ":"
        ++ (paddedString <| Time.toMinute zone posix)


paddedString : Int -> String
paddedString =
    String.padLeft 2 '0' << String.fromInt


formatMonth : Zone -> Time.Posix -> String
formatMonth zone posix =
    case Time.toMonth zone posix of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"

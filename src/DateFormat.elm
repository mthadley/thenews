module DateFormat exposing (format)

import Date exposing (Date)


format : Int -> String
format time =
    formatDate <| Date.fromTime <| toFloat <| time * 1000


formatDate : Date -> String
formatDate date =
    (toString <| Date.month date)
        ++ " "
        ++ (toString <| Date.day date)
        ++ ", "
        ++ (toString <| Date.year date)
        ++ " at "
        ++ (paddedString <| Date.hour date)
        ++ ":"
        ++ (paddedString <| Date.minute date)


paddedString : Int -> String
paddedString =
    String.padLeft 2 '0' << toString

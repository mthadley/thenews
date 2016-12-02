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
        ++ (toString <| Date.hour date)
        ++ ":"
        ++ (toString <| Date.minute date)

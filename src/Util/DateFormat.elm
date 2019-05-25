module Util.DateFormat exposing (format)

import Time


format : Int -> String
format =
    formatDate << Time.millisToPosix


formatDate : Time.Posix -> String
formatDate date =
    "TODO: Fix time format"



-- (toString <| Date.month date)
--     ++ " "
--     ++ (toString <| Date.day date)
--     ++ ", "
--     ++ (toString <| Date.year date)
--     ++ " at "
--     ++ (paddedString <| Date.hour date)
--     ++ ":"
--     ++ (paddedString <| Date.minute date)


paddedString : Int -> String
paddedString =
    String.padLeft 2 '0' << String.fromInt

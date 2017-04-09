port module PageTitle exposing (set)


port setTitle : String -> Cmd msg


set : String -> Cmd msg
set title =
    setTitle <| "TN - " ++ title

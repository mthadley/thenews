module Data.Category exposing (Category(..), toString)


type Category
    = Ask
    | Best
    | Job
    | New
    | Show
    | Top


toString : Category -> String
toString category =
    case category of
        Ask ->
            "Ask"

        Best ->
            "Best"

        Job ->
            "Job"

        New ->
            "New"

        Show ->
            "Show"

        Top ->
            "Top"

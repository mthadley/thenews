module Data.Category exposing (Category(..), sorter, toString)

import Sort


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


sorter : Sort.Sorter Category
sorter =
    Sort.by toString Sort.alphabetical

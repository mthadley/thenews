module Pages.NotFound exposing (view)

import Html.Styled exposing (..)


view : Html msg
view =
    div []
        [ h1 [] [ text "Error: 404 Not Found" ]
        , p [] [ text "Sorry, but I'm not sure what you were looking for." ]
        ]

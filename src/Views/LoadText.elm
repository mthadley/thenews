module Views.LoadText exposing (Model, Msg, init, subscriptions, update, view, viewString)

import Html.Styled exposing (..)
import Time



-- MODEL


type alias Model =
    { count : Int
    }


init : Model
init =
    Model 0



-- VIEW


view : Model -> Html msg
view model =
    div [] [ text <| viewString model ]


viewString : Model -> String
viewString model =
    "Loading" ++ String.repeat model.count "."



--UPDATE


type Msg
    = Tick


update : Msg -> Model -> Model
update Tick model =
    { model | count = modBy 4 (model.count + 1) }


subscriptions : Bool -> Sub Msg
subscriptions loading =
    if loading then
        Time.every 250 <| always Tick

    else
        Sub.none

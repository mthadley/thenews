module LoadText exposing (Model, Msg, init, subscriptions, toggle, update, view)

import Html.Styled exposing (..)
import Time


-- MODEL


type alias Model =
    { count : Int
    , animating : Bool
    }


init : Bool -> Model
init =
    Model 0



-- VIEW


view : Model -> Html msg
view model =
    div [] [ text <| "Loading" ++ (String.repeat model.count ".") ]



--UPDATE


type Msg
    = Tick


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            { model | count = rem (model.count + 1) 4 }


toggle : Bool -> Model -> Model
toggle on model =
    { model | animating = on }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.animating then
        Time.every (250 * Time.millisecond) <| always Tick
    else
        Sub.none

port module Theme.Preference exposing
    ( Preference
    , Selection(..)
    , currentSelection
    , decode
    , init
    , none
    , select
    , subscriptions
    , theme
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Theme exposing (Theme(..))


type Preference
    = Auto Theme
    | Theme { system : Theme, chosen : Theme }


type Selection
    = SelectTheme Theme
    | SelectAuto


{-| Useful as a default value.
-}
none : Preference
none =
    Auto Dark


select : Selection -> Preference -> ( Preference, Cmd msg )
select selection preference =
    case ( selection, preference ) of
        ( SelectAuto, Auto _ ) ->
            ( preference, Cmd.none )

        ( SelectAuto, Theme { system } ) ->
            ( Auto system, setThemePreference Nothing )

        ( SelectTheme theme_, Auto system ) ->
            ( Theme { system = system, chosen = theme_ }
            , setThemePreference (Just <| Theme.toString theme_)
            )

        ( SelectTheme theme_, Theme info ) ->
            ( Theme { info | chosen = theme_ }
            , setThemePreference (Just <| Theme.toString theme_)
            )


currentSelection : Preference -> Selection
currentSelection preference =
    case preference of
        Auto _ ->
            SelectAuto

        Theme { chosen } ->
            SelectTheme chosen


theme : Preference -> Theme
theme preference =
    case preference of
        Auto system ->
            system

        Theme { chosen } ->
            chosen


type alias Flags =
    { systemTheme : Theme
    , themePreference : Maybe Theme
    }


decode : Decoder Preference
decode =
    Decode.map init decodeFlags


decodeFlags : Decoder Flags
decodeFlags =
    Decode.succeed Flags
        |> required "systemTheme"
            (Decode.string
                |> Decode.map (Maybe.withDefault Dark << Theme.fromString)
            )
        |> required "themePreference"
            (Decode.string
                |> Decode.nullable
                |> Decode.map (Maybe.andThen Theme.fromString)
            )


init : Flags -> Preference
init flags =
    case flags.themePreference of
        Nothing ->
            Auto flags.systemTheme

        Just preference ->
            Theme { system = flags.systemTheme, chosen = preference }


subscriptions : Preference -> Sub Preference
subscriptions currentPreference =
    currentTheme
        (Theme.fromString
            >> Maybe.withDefault Theme.Dark
            >> update currentPreference
        )


update : Preference -> Theme -> Preference
update currentPreference newSystemTheme =
    case currentPreference of
        Auto _ ->
            Auto newSystemTheme

        Theme info ->
            Theme { info | system = newSystemTheme }



-- Ports


port currentTheme : (String -> msg) -> Sub msg


port setThemePreference : Maybe String -> Cmd msg

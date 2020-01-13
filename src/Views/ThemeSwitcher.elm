module Views.ThemeSwitcher exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Theme exposing (Theme(..))
import Theme.Preference as ThemePreference exposing (Preference, Selection(..))


view : Preference -> Html Selection
view preference =
    let
        currentSelection =
            ThemePreference.currentSelection preference

        colors =
            Theme.colors (ThemePreference.theme preference)
    in
    button
        [ onClick (nextSelection currentSelection)
        , css
            [ borderWidth zero
            , backgroundColor transparent
            , color colors.secondary
            , cursor pointer
            , fontSize inherit
            , fontFamily inherit
            ]
        ]
        [ text <| selectionLabel currentSelection ]


nextSelection : Selection -> Selection
nextSelection selection =
    case selection of
        SelectAuto ->
            SelectTheme Light

        SelectTheme Light ->
            SelectTheme Dark

        SelectTheme Dark ->
            SelectAuto


selectionLabel : Selection -> String
selectionLabel selection =
    case selection of
        SelectAuto ->
            "Auto"

        SelectTheme Light ->
            "Light"

        SelectTheme Dark ->
            "Dark"

module ExamplesPanel exposing (examplesPanel, toggleExamplesPanel)

import BuilderCss exposing (CssClasses(..), cssNamespace, panelStyling)
import Css
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import Types
    exposing
        ( Msg(..)
        , ParameterRecord
        , InputValues
        , Parameter(..)
        , Panel(..)
        )


{ class, classList, id } =
    Html.CssHelpers.withNamespace cssNamespace


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


examplesPanel : Html Msg
examplesPanel =
    div
        [ class [ OverlayPanelCss ]
        , id [ ExamplesPanel ]
        , styles <| panelStyling ++ examplesPanelStyling
        ]
        [ h3 [] [ text "Examples" ]
        , button
            [ styles exampleButtonStyling
            , onClick <| SetParametersAndBuild basisSetDimer
            ]
            [ text "Dimer" ]
        , br [] []
        , button
            [ styles exampleButtonStyling
            , onClick <| SetParametersAndBuild basisSetTrimer
            ]
            [ text "Trimer" ]
        , br [] []
        , button
            [ styles exampleButtonStyling
            , onClick <| SetParametersAndBuild basisSetTetramer
            ]
            [ text "Tetramer" ]
        ]


examplesPanelStyling : List Css.Mixin
examplesPanelStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 30)
    ]


exampleButtonStyling : List Css.Mixin
exampleButtonStyling =
    [ Css.width (Css.pct 80)
    ]


basisSetDimer : ParameterRecord
basisSetDimer =
    { oligomerState = Just 2
    , radius = Just 5.1
    , pitch = Just 226
    , phiCA = Just 24
    , sequence = Just "EIAALKQEIAALKKENAALKWEIAALKQ"
    , register = "g"
    }


basisSetTrimer : ParameterRecord
basisSetTrimer =
    { oligomerState = Just 3
    , radius = Just 6.3
    , pitch = Just 194
    , phiCA = Just 20.0
    , sequence = Just "EIAAIKQEIAAIKKEIAAIKWEIAAIKQ"
    , register = "g"
    }


basisSetTetramer : ParameterRecord
basisSetTetramer =
    { oligomerState = Just 4
    , radius = Just 6.8
    , pitch = Just 213
    , phiCA = Just 22.1
    , sequence = Just "ELAAIKQELAAIKKELAAIKWELAAIKQ"
    , register = "g"
    }


toggleExamplesPanel : Html Msg
toggleExamplesPanel =
    div
        [ class [ OverlayPanelCss, PanelToggleCss ]
        , onClick (TogglePanel ExamplesPanel)
        ]
        [ text "Examples" ]
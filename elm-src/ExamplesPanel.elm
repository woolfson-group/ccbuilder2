module ExamplesPanel exposing (examplesPanel, toggleExamplesPanel)

import BuilderCss exposing (CssClasses(..), cssNamespace, panelStyling)
import Css
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import Svg
import Svg.Attributes as SvgAtt
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
            [ onClick <| SetParametersAndBuild basisSetDimer
            ]
            [ dimerIcon 50 50 ]
        , br [] []
        , button
            [ onClick <| SetParametersAndBuild basisSetTrimer
            ]
            [ text "Trimer" ]
        , br [] []
        , button
            [ onClick <| SetParametersAndBuild basisSetTetramer
            ]
            [ text "Tetramer" ]
        ]


toStringRound =
    toString << round


x1f =
    SvgAtt.x1 << toStringRound


y1f =
    SvgAtt.y1 << toStringRound


x2f =
    SvgAtt.x2 << toStringRound


y2f =
    SvgAtt.y2 << toStringRound


cxf =
    SvgAtt.cx << toStringRound


cyf =
    SvgAtt.cy << toStringRound


rf =
    SvgAtt.r << toStringRound


dimerIcon : Float -> Float -> Html msg
dimerIcon width height =
    Svg.svg
        [ SvgAtt.width "50px"
        , SvgAtt.height "50px"
        , SvgAtt.display "block"
        ]
        [ Svg.line
            [ x1f (width * (1 / 2))
            , y1f (height * (1 / 4))
            , x2f (width * (1 / 2))
            , y2f (height * (3 / 4))
            , SvgAtt.strokeWidth <| toString 2
            , SvgAtt.stroke "black"
            ]
            []
        , Svg.circle
            [ cxf (width * (1 / 2))
            , cyf (height * (1 / 4))
            , rf 5
            , SvgAtt.strokeWidth <| toString 2
            , SvgAtt.stroke "black"
            , SvgAtt.fill "grey"
            ]
            []
        , Svg.circle
            [ cxf (width * (1 / 2))
            , cyf (height * (3 / 4))
            , rf 5
            , SvgAtt.strokeWidth <| toString 2
            , SvgAtt.stroke "black"
            , SvgAtt.fill "grey"
            ]
            []
        ]


examplesPanelStyling : List Css.Mixin
examplesPanelStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 30)
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

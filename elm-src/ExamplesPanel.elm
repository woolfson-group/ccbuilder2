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


tau : Float
tau = 2 * pi


toStringRound : Float -> String
toStringRound = toString << round


x1f : Float -> Svg.Attribute msg
x1f = SvgAtt.x1 << toStringRound


y1f : Float -> Svg.Attribute msg
y1f = SvgAtt.y1 << toStringRound


x2f : Float -> Svg.Attribute msg
x2f = SvgAtt.x2 << toStringRound


y2f : Float -> Svg.Attribute msg
y2f = SvgAtt.y2 << toStringRound


cxf : Float -> Svg.Attribute msg
cxf = SvgAtt.cx << toStringRound


cyf : Float -> Svg.Attribute msg
cyf = SvgAtt.cy << toStringRound


rf : Float -> Svg.Attribute msg
rf = SvgAtt.r << toStringRound


dimerIcon : Float -> Html msg
dimerIcon widthAndHeight =
    Svg.svg
        [ SvgAtt.width "50px"
        , SvgAtt.height "50px"
        , SvgAtt.display "block"
        ]
        [ Svg.line
            [ x1f (widthAndHeight * (1 / 2))
            , y1f (widthAndHeight * (1 / 4))
            , x2f (widthAndHeight * (1 / 2))
            , y2f (widthAndHeight * (3 / 4))
            , SvgAtt.strokeWidth <| toString 2
            , SvgAtt.stroke "black"
            ]
            []
        , drawHelixCircle (widthAndHeight * (1 / 2)) (widthAndHeight * (1 / 4)) 5
        , drawHelixCircle (widthAndHeight * (1 / 2)) (widthAndHeight * (3 / 4)) 5
        ]


angleToCoordinate r theta 


drawHelixCircle : Float -> Float -> Float -> Svg.Svg msg
drawHelixCircle x y r =
    Svg.circle
        [ cxf x
        , cyf y
        , rf r
        , SvgAtt.strokeWidth <| toString 2
        , SvgAtt.stroke "black"
        , SvgAtt.fill "grey"
        ]
        []


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

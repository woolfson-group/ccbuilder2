module ExamplesPanel exposing (examplesPanel, toggleExamplesPanel, basisSetDimer)

import BuilderCss exposing (CssClasses(..), cssNamespace, panelStyling)
import Css
import Dict
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
        , ParametersDict
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
        [ h2 [] [ text "Examples" ]
        , div []
            [ h3 [] [ text "Basis Set" ]
            , text "CC Di"
            , br [] []
            , button
                [ onClick <| SetParametersAndBuild basisSetDimer
                , styles buttonStyling
                ]
                [ ccIcon 2 60 ]
            , br [] []
            , text "CC Tri"
            , br [] []
            , button
                [ onClick <| SetParametersAndBuild basisSetTrimer
                , styles buttonStyling
                ]
                [ ccIcon 3 60 ]
            , br [] []
            , text "CC Tet"
            , br [] []
            , button
                [ onClick <| SetParametersAndBuild basisSetTetramer
                , styles buttonStyling
                ]
                [ ccIcon 4 60 ]
            ]
        , div []
            [ h3 [] [ text "α-Helical Barrels" ]
            , text "CC Pent"
            , br [] []
            , button
                [ onClick <| SetParametersAndBuild largermerCCPent
                , styles buttonStyling
                ]
                [ ccIcon 5 60 ]
            , br [] []
            , text "CC Hex"
            , br [] []
            , button
                [ onClick <| SetParametersAndBuild largermerCCHex
                , styles buttonStyling
                ]
                [ ccIcon 6 60 ]
            , br [] []
            , text "CC Hex2"
            , br [] []
            , button
                [ onClick <| SetParametersAndBuild largermerCCHex2
                , styles buttonStyling
                ]
                [ ccIcon 6 60 ]
            , br [] []
            , text "CC Hex3"
            , br [] []
            , button
                [ onClick <| SetParametersAndBuild largermerCCHex3
                , styles buttonStyling
                ]
                [ ccIcon 6 60 ]
            , br [] []
            , text "CC Hept"
            , br [] []
            , button
                [ onClick <| SetParametersAndBuild largermerCCHept
                , styles buttonStyling
                ]
                [ ccIcon 7 60 ]
            ]
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


ccIcon : Int -> Float -> Html msg
ccIcon n widthAndHeight =
    let
        r = widthAndHeight*0.5
        deltaAngle = tau / (toFloat n)
        frameCoordinates =
            List.map (\v -> ((toFloat v) * deltaAngle) + (tau/4)) (List.range 0 n)
            |> List.map (\a -> fromPolar (r * ( Basics.max 0.5 (0.11 * (toFloat n))), a))
            |> List.map (\(x, y) -> (x + r, r - y))
        coordinatePairs = makeCoordinatePairs frameCoordinates
    in
        Svg.svg
            [ SvgAtt.width <| toString widthAndHeight
            , SvgAtt.height <| toString widthAndHeight
            , SvgAtt.display "block"
            ]
            (
                (List.map (drawHelixCircle 6) frameCoordinates)
                |> List.append (List.map drawLine coordinatePairs)
            )


drawHelixCircle : Float -> (Float, Float) -> Svg.Svg msg
drawHelixCircle r (x, y) =
    Svg.circle
        [ cxf x
        , cyf y
        , rf r
        , SvgAtt.strokeWidth <| toString 2
        , SvgAtt.stroke "black"
        , SvgAtt.fill "grey"
        ]
        []


makeCoordinatePairs : List (Float, Float) -> List ((Float, Float), (Float, Float))
makeCoordinatePairs helixCentres =
    let
        hcl = List.length helixCentres
        front = List.take (hcl - 1) helixCentres
        back = Maybe.withDefault [] <| List.tail helixCentres
    in
        List.map2 (,) front back


drawLine : ((Float, Float), (Float, Float)) -> Svg.Svg msg
drawLine ((x1, y1), (x2, y2)) =
    Svg.line
        [ x1f x1
        , y1f y1
        , x2f x2
        , y2f y2
        , SvgAtt.strokeWidth <| toString 2
        , SvgAtt.stroke "black"
        ]
        []


examplesPanelStyling : List Css.Mixin
examplesPanelStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 35)
    ]


buttonStyling : List Css.Mixin
buttonStyling =
    [ Css.margin (Css.px 5)
    , Css.borderRadius (Css.px 10)
    ]


basisSetDimer : ParametersDict
basisSetDimer =
    makeHomoOligomerExample 2
        { radius = Just 5.1
        , pitch = Just 226
        , phiCA = Just 24
        , sequence = Just "EIAALKQEIAALKKENAALKWEIAALKQ"
        , register = "g"
        }


basisSetTrimer : ParametersDict
basisSetTrimer =
    makeHomoOligomerExample 3
        { radius = Just 6.3
        , pitch = Just 194
        , phiCA = Just 20.0
        , sequence = Just "EIAAIKQEIAAIKKEIAAIKWEIAAIKQ"
        , register = "g"
        }


basisSetTetramer : ParametersDict
basisSetTetramer =
    makeHomoOligomerExample 4
        { radius = Just 6.8
        , pitch = Just 213
        , phiCA = Just 22.1
        , sequence = Just "ELAAIKQELAAIKKELAAIKWELAAIKQ"
        , register = "g"
        }


largermerCCPent : ParametersDict
largermerCCPent =
    makeHomoOligomerExample 5
        { radius = Just 8.6
        , pitch = Just 183
        , phiCA = Just 14.4
        , sequence = Just "KIEQILQKIEKILQKIEWILQKIEQILQ"
        , register = "c"
        }


largermerCCHex : ParametersDict
largermerCCHex =
    makeHomoOligomerExample 6
        { radius = Just 9.1
        , pitch = Just 228
        , phiCA = Just 16.4
        , sequence = Just "ELKAIAQELKAIAKELKAIAWELKAIAQ"
        , register = "g"
        }


largermerCCHex2 : ParametersDict
largermerCCHex2 =
    makeHomoOligomerExample 6
        { radius = Just 9.5
        , pitch = Just 162
        , phiCA = Just 18.2
        , sequence = Just "EIAKSLKEIAKSLKEIAWSLKEIAKSLK"
        , register = "c"
        }


largermerCCHex3 : ParametersDict
largermerCCHex3 =
    makeHomoOligomerExample 6
        { radius = Just 9.7
        , pitch = Just 132
        , phiCA = Just 13.1
        , sequence = Just "EIAQSIKEIAKSIKEIAWSIKEIAQSIK"
        , register = "c"
        }


largermerCCHept : ParametersDict
largermerCCHept =
    makeHomoOligomerExample 7
        { radius = Just 9.8
        , pitch = Just 329
        , phiCA = Just 15.1
        , sequence = Just "EIAQALKEIAKALKEIAWALKEIAQALK"
        , register = "c"
        }

makeHomoOligomerExample : Int -> ParameterRecord -> ParametersDict
makeHomoOligomerExample oligomericState parameters =
    List.map2 (,) (List.range 1 oligomericState) (List.repeat oligomericState parameters)
    |> Dict.fromList


toggleExamplesPanel : Html Msg
toggleExamplesPanel =
    div
        [ class [ OverlayPanelCss, LeftPanelToggleCss  ]
        , onClick (TogglePanel ExamplesPanel)
        ]
        [ text "Examples" ]

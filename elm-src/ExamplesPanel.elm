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


examplesPanel : Bool -> Bool -> Html Msg
examplesPanel building visible =
    div
        [ class [ OverlayPanelCss ]
        , id [ ExamplesPanel ]
        , styles <| panelStyling ++ examplesPanelStyling
        , hidden <| not visible
        ]
        [ h2 [] [ text "Examples" ]
        , div []
            [ hr [] []
            , h3 [] [ text "Basis Set" ]
            , hr [] []
            , exampleButton 2 basisSetDimer building
            , br [] []
            , text "CC Di"
            , br [] []
            , exampleButton 3 basisSetTrimer building
            , br [] []
            , text "CC Tri"
            , br [] []
            , exampleButton 4 basisSetTetramer building
            , br [] []
            , text "CC Tet"
            ]
        , div []
            [ hr [] []
            , h3 [] [ text "α-Helical Barrels" ]
            , hr [] []
            , exampleButton 5 largermerCCPent building
            , br [] []
            , text "CC Pent"
            , br [] []
            , exampleButton 6 largermerCCHex building
            , br [] []
            , text "CC Hex"
            , br [] []
            , exampleButton 6 largermerCCHex2 building
            , br [] []
            , text "CC Hex2"
            , br [] []
            , exampleButton 6 largermerCCHex3 building
            , br [] []
            , text "CC Hex3"
            , br [] []
            , exampleButton 7 largermerCCHept building
            , br [] []
            , text "CC Hept"
            ]
        ]


exampleButton : Int -> ParametersDict -> Bool -> Html Msg
exampleButton os exampleParameters building =
    button
        [ class [ CCBButtonCss ]
        , onClick <| SetParametersAndBuild exampleParameters
        , styles buttonStyling
        , disabled building
        ]
        [ ccIcon os 60 ]


tau : Float
tau =
    2 * pi


toStringRound : Float -> String
toStringRound =
    toString << round


x1f : Float -> Svg.Attribute msg
x1f =
    SvgAtt.x1 << toStringRound


y1f : Float -> Svg.Attribute msg
y1f =
    SvgAtt.y1 << toStringRound


x2f : Float -> Svg.Attribute msg
x2f =
    SvgAtt.x2 << toStringRound


y2f : Float -> Svg.Attribute msg
y2f =
    SvgAtt.y2 << toStringRound


cxf : Float -> Svg.Attribute msg
cxf =
    SvgAtt.cx << toStringRound


cyf : Float -> Svg.Attribute msg
cyf =
    SvgAtt.cy << toStringRound


rf : Float -> Svg.Attribute msg
rf =
    SvgAtt.r << toStringRound


ccIcon : Int -> Float -> Html msg
ccIcon n widthAndHeight =
    let
        r =
            widthAndHeight * 0.5

        deltaAngle =
            tau / (toFloat n)

        frameCoordinates =
            List.map (\v -> ((toFloat v) * deltaAngle) + (tau / 4)) (List.range 0 n)
                |> List.map (\a -> fromPolar ( r * (Basics.max 0.5 (0.11 * (toFloat n))), a ))
                |> List.map (\( x, y ) -> ( x + r, r - y ))

        coordinatePairs =
            makeCoordinatePairs frameCoordinates
    in
        Svg.svg
            [ SvgAtt.width <| toString widthAndHeight
            , SvgAtt.height <| toString widthAndHeight
            , SvgAtt.display "block"
            ]
            ((List.map (drawHelixCircle 6) frameCoordinates)
                |> List.append (List.map drawLine coordinatePairs)
            )


drawHelixCircle : Float -> ( Float, Float ) -> Svg.Svg msg
drawHelixCircle r ( x, y ) =
    Svg.circle
        [ cxf x
        , cyf y
        , rf r
        , SvgAtt.strokeWidth <| toString 2
        , SvgAtt.stroke "black"
        , SvgAtt.fill "grey"
        ]
        []


makeCoordinatePairs : List ( Float, Float ) -> List ( ( Float, Float ), ( Float, Float ) )
makeCoordinatePairs helixCentres =
    let
        hcl =
            List.length helixCentres

        front =
            List.take (hcl - 1) helixCentres

        back =
            Maybe.withDefault [] <| List.tail helixCentres
    in
        List.map2 (,) front back


drawLine : ( ( Float, Float ), ( Float, Float ) ) -> Svg.Svg msg
drawLine ( ( x1, y1 ), ( x2, y2 ) ) =
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
        , superHelRot = Just 0.0
        , antiParallel = False
        , zShift = Just 0.0
        , linkedSuperHelRot = True
        }


basisSetTrimer : ParametersDict
basisSetTrimer =
    makeHomoOligomerExample 3
        { radius = Just 6.3
        , pitch = Just 194
        , phiCA = Just 20.0
        , sequence = Just "EIAAIKQEIAAIKKEIAAIKWEIAAIKQ"
        , register = "g"
        , superHelRot = Just 0.0
        , antiParallel = False
        , zShift = Just 0.0
        , linkedSuperHelRot = True
        }


basisSetTetramer : ParametersDict
basisSetTetramer =
    makeHomoOligomerExample 4
        { radius = Just 6.8
        , pitch = Just 213
        , phiCA = Just 22.1
        , sequence = Just "ELAAIKQELAAIKKELAAIKWELAAIKQ"
        , register = "g"
        , superHelRot = Just 0.0
        , antiParallel = False
        , zShift = Just 0.0
        , linkedSuperHelRot = True
        }


largermerCCPent : ParametersDict
largermerCCPent =
    makeHomoOligomerExample 5
        { radius = Just 8.6
        , pitch = Just 183
        , phiCA = Just 14.4
        , sequence = Just "KIEQILQKIEKILQKIEWILQKIEQILQ"
        , register = "c"
        , superHelRot = Just 0.0
        , antiParallel = False
        , zShift = Just 0.0
        , linkedSuperHelRot = True
        }


largermerCCHex : ParametersDict
largermerCCHex =
    makeHomoOligomerExample 6
        { radius = Just 9.1
        , pitch = Just 228
        , phiCA = Just 16.4
        , sequence = Just "ELKAIAQELKAIAKELKAIAWELKAIAQ"
        , register = "g"
        , superHelRot = Just 0.0
        , antiParallel = False
        , zShift = Just 0.0
        , linkedSuperHelRot = True
        }


largermerCCHex2 : ParametersDict
largermerCCHex2 =
    makeHomoOligomerExample 6
        { radius = Just 9.5
        , pitch = Just 162
        , phiCA = Just 18.2
        , sequence = Just "EIAKSLKEIAKSLKEIAWSLKEIAKSLK"
        , register = "c"
        , superHelRot = Just 0.0
        , antiParallel = False
        , zShift = Just 0.0
        , linkedSuperHelRot = True
        }


largermerCCHex3 : ParametersDict
largermerCCHex3 =
    makeHomoOligomerExample 6
        { radius = Just 9.7
        , pitch = Just 132
        , phiCA = Just 13.1
        , sequence = Just "EIAQSIKEIAKSIKEIAWSIKEIAQSIK"
        , register = "c"
        , superHelRot = Just 0.0
        , antiParallel = False
        , zShift = Just 0.0
        , linkedSuperHelRot = True
        }


largermerCCHept : ParametersDict
largermerCCHept =
    makeHomoOligomerExample 7
        { radius = Just 9.8
        , pitch = Just 329
        , phiCA = Just 15.1
        , sequence = Just "EIAQALKEIAKALKEIAWALKEIAQALK"
        , register = "c"
        , superHelRot = Just 0.0
        , antiParallel = False
        , zShift = Just 0.0
        , linkedSuperHelRot = True
        }


makeHomoOligomerExample : Int -> ParameterRecord -> ParametersDict
makeHomoOligomerExample oligomericState parameters =
    List.map2 (,) (List.range 1 oligomericState) (List.repeat oligomericState parameters)
        |> Dict.fromList


toggleExamplesPanel : Html Msg
toggleExamplesPanel =
    div
        [ class [ OverlayPanelCss, LeftPanelToggleCss ]
        , onClick (TogglePanel ExamplesPanel)
        ]
        [ text "Examples" ]

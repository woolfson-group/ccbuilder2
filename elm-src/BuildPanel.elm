module BuildPanel exposing (..)

import BuilderCss exposing (CssClasses(..), cssNamespace, panelStyling)
import Css
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import ParameterValidation exposing (containsInvalidParameter, editParameterValue)
import Types
    exposing
        ( Msg(..)
        , ParameterRecord
        , PRID
        , ParametersDict
        , emptyParameterRecord
        , IVID
        , InputValuesDict
        , InputValues
        , Parameter(..)
        , Panel(..)
        )


{ class, classList, id } =
    Html.CssHelpers.withNamespace cssNamespace


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


buildPanel : ParametersDict -> InputValuesDict -> Html Msg
buildPanel parametersDict currentInputDict =
    div [ class [ OverlayPanelCss ], id [ BuildPanel ], styles <| panelStyling ++ buildPanelStyling ]
        [ h3 [] [ text "Parameters" ]
        , parameterInputForm parametersDict currentInputDict
        ]


buildPanelStyling : List Css.Mixin
buildPanelStyling =
    [ Css.top (Css.px 60), Css.left (Css.px 30) ]


parameterInputForm : ParametersDict -> InputValuesDict -> Html Msg
parameterInputForm parametersDict currentInputDict =
    Html.div []
        (createParametersSections currentInputDict
            ++ [ parameterSubmit parametersDict
               , button [ onClick Clear ] [ text "Clear" ]
               ]
        )


createParametersSections : InputValuesDict -> List (Html Msg)
createParametersSections currentInputDict =
    Dict.values currentInputDict
    |> List.map chainInputSection


chainInputSection : InputValues -> Html Msg
chainInputSection currentInput =
    List.map parameterInput (allParameters currentInput)
        ++ [ sequenceInput
                ( "Sequence", Sequence, currentInput.sequence, currentInput.register )
           ]
    |> div []


allParameters : InputValues -> List ( String, Parameter, String )
allParameters currentInput =
    [ ( "Oligomer State", OligomerState, currentInput.oligomerState )
    , ( "Radius", Radius, currentInput.radius )
    , ( "Pitch", Pitch, currentInput.pitch )
    , ( "Interface Angle", PhiCA, currentInput.phiCA )
    ]


parameterInput : ( String, Parameter, String ) -> Html Msg
parameterInput ( parameterLabel, parameter, currentParameter ) =
    div [ class [ ParameterInputCss ] ]
        [ text parameterLabel
        , br [] []
        , input
            [ type_ "text"
            , name parameterLabel
            , placeholder parameterLabel
            , onInput (EditParameter parameter)
            , styles inputStyling
            , value currentParameter
            ]
            []
        ]


sequenceInput : ( String, Parameter, String, String ) -> Html Msg
sequenceInput ( parameterLabel, parameter, currentSequence, currentRegister ) =
    div [ class [ ParameterInputCss ] ]
        [ text parameterLabel
        , text " (Register: "
        , registerSelection currentRegister
        , text ")"
        , br [] []
        , textarea
            [ name parameterLabel
            , rows 3
            , cols 30
            , styles inputStyling
            , placeholder parameterLabel
            , onInput (EditParameter parameter)
            , value currentSequence
            ]
            []
        ]


inputStyling : List Css.Mixin
inputStyling =
    [ Css.width (Css.pct 100) ]


parameterSubmit : ParametersDict -> Html Msg
parameterSubmit parameters =
    input
        [ type_ "submit"
        , value "Submit"
        , onClick Build
        , disabled (sumbitDisabled parameters)
        ]
        []


sumbitDisabled : ParametersDict -> Bool
sumbitDisabled parameters =
    Dict.values parameters
    |> List.map containsInvalidParameter
    |> List.all (\v -> v == True)


registerSelection : String -> Html Msg
registerSelection currentRegister =
    select
        [ value currentRegister, onInput (EditParameter Register) ]
        (List.map registerOption [ "a", "b", "c", "d", "e", "f", "g" ])


registerOption : String -> Html msg
registerOption register =
    option [ value register ] [ text register ]


toggleBuildPanel : Html Msg
toggleBuildPanel =
    div
        [ class [ OverlayPanelCss, PanelToggleCss ]
        , onClick (TogglePanel BuildPanel)
        ]
        [ text "Build" ]

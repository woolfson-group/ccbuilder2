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
        , SectionID
        , ParametersDict
        , emptyParameterRecord
        , InputValuesDict
        , InputValues
        , Parameter(..)
        , BuildMode(..)
        , Panel(..)
        , emptyInput
        )


{ class, classList, id } =
    Html.CssHelpers.withNamespace cssNamespace


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


buildPanel : BuildMode -> ParametersDict -> InputValuesDict -> Html Msg
buildPanel buildMode parametersDict currentInputDict =
    let
        panelView =
            case buildMode of
                Basic ->
                    basicParameterInputForm

                Advanced ->
                    advancedParameterInputForm
    in
        div
            [ class [ OverlayPanelCss ]
            , id [ BuildPanel ]
            , styles <| panelStyling ++ buildPanelStyling
            ]
            [ h2 [] [ text "Build" ]
            , selectBuildMode buildMode
            , br [] []
            , h3 [] [ text "Oligomeric State" ]
            , selectOligomericState (Dict.toList parametersDict |> List.length)
            , panelView parametersDict currentInputDict
            ]


selectBuildMode : BuildMode -> Html Msg
selectBuildMode currentBuildMode =
    let
        buildModeValue =
            case currentBuildMode of
                Basic ->
                    "Basic"

                Advanced ->
                    "Advanced"
    in
        select
            [ value buildModeValue
            , onInput ChangeBuildMode
            ]
            (List.map simpleOption [ "Basic", "Advanced" ])


selectOligomericState : Int -> Html Msg
selectOligomericState currentOS =
    select
        [ value (toString currentOS)
        , onInput SetOligomericState
        ]
        (List.map (toString >> simpleOption) (List.range 2 30) )


simpleOption : String -> Html msg
simpleOption optionValue =
    option [ value optionValue ] [ text optionValue ]


buildPanelStyling : List Css.Mixin
buildPanelStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 35) ]


allParameters : InputValues -> List ( String, Parameter, String )
allParameters currentInput =
    [ ( "Radius", Radius, currentInput.radius )
    , ( "Pitch", Pitch, currentInput.pitch )
    , ( "Interface Angle", PhiCA, currentInput.phiCA )
    ]


basicParameterInputForm : ParametersDict -> InputValuesDict -> Html Msg
basicParameterInputForm parametersDict currentInputDict =
    Html.div []
        [ h3 [] [ text "Parameters" ]
        , Dict.get 1 currentInputDict
            |> Maybe.withDefault emptyInput
            |> allChainInputSection
        , parameterSubmit parametersDict
        , button [ onClick Clear ] [ text "Clear" ]
        ]


allChainInputSection : InputValues -> Html Msg
allChainInputSection currentInput =
    List.map allParameterInput (allParameters currentInput)
        ++ [ allSequenceInput
                ( "Sequence", Sequence, currentInput.sequence, currentInput.register )
           ]
        |> div [ class [ FlexItemCss ] ]


allParameterInput : ( String, Parameter, String ) -> Html Msg
allParameterInput ( parameterLabel, parameter, currentParameter ) =
    div [ class [ ParameterInputCss ] ]
        [ text parameterLabel
        , br [] []
        , input
            [ type_ "text"
            , name parameterLabel
            , placeholder parameterLabel
            , onInput (EditAllParameters parameter)
            , styles inputStyling
            , value currentParameter
            ]
            []
        ]


allSequenceInput : ( String, Parameter, String, String ) -> Html Msg
allSequenceInput ( parameterLabel, parameter, currentSequence, currentRegister ) =
    div [ class [ ParameterInputCss ] ]
        [ text parameterLabel
        , text " (Register: "
        , registerSelection 1 currentRegister
        , text ")"
        , br [] []
        , textarea
            [ name parameterLabel
            , rows 3
            , cols 30
            , styles inputStyling
            , placeholder parameterLabel
            , onInput (EditAllParameters parameter)
            , value currentSequence
            ]
            []
        ]


advancedParameterInputForm : ParametersDict -> InputValuesDict -> Html Msg
advancedParameterInputForm parametersDict currentInputDict =
    Html.div []
        [ h3 [] [ text "Parameters" ]
        , Html.div [ class [ FlexContainerCss ] ]
            (createParametersSections currentInputDict)
        , parameterSubmit parametersDict
        , button [ onClick Clear ] [ text "Clear" ]
        ]


createParametersSections : InputValuesDict -> List (Html Msg)
createParametersSections currentInputDict =
    Dict.toList currentInputDict
        |> List.map singleChainInputSection


singleChainInputSection : ( SectionID, InputValues ) -> Html Msg
singleChainInputSection ( sectionID, currentInput ) =
    List.map (singleParameterInput sectionID) (allParameters currentInput)
        ++ [ singleSequenceInput sectionID
                ( "Sequence", Sequence, currentInput.sequence, currentInput.register )
           ]
        |> div [ class [ FlexItemCss ] ]


singleParameterInput : SectionID -> ( String, Parameter, String ) -> Html Msg
singleParameterInput sectionID ( parameterLabel, parameter, currentParameter ) =
    div [ class [ ParameterInputCss ] ]
        [ text parameterLabel
        , br [] []
        , input
            [ type_ "text"
            , name parameterLabel
            , placeholder parameterLabel
            , onInput (EditSingleParameter parameter sectionID)
            , styles inputStyling
            , value currentParameter
            ]
            []
        ]


singleSequenceInput : SectionID -> ( String, Parameter, String, String ) -> Html Msg
singleSequenceInput sectionID ( parameterLabel, parameter, currentSequence, currentRegister ) =
    div [ class [ ParameterInputCss ] ]
        [ text parameterLabel
        , text " (Register: "
        , registerSelection sectionID currentRegister
        , text ")"
        , br [] []
        , textarea
            [ name parameterLabel
            , rows 3
            , cols 30
            , styles inputStyling
            , placeholder parameterLabel
            , onInput (EditSingleParameter parameter sectionID)
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


registerSelection : SectionID -> String -> Html Msg
registerSelection sectionID currentRegister =
    select
        [ value currentRegister, onInput (EditSingleParameter Register sectionID) ]
        (List.map simpleOption [ "a", "b", "c", "d", "e", "f", "g" ])


toggleBuildPanel : Html Msg
toggleBuildPanel =
    div
        [ class [ OverlayPanelCss, LeftPanelToggleCss ]
        , onClick (TogglePanel BuildPanel)
        ]
        [ text "Build" ]

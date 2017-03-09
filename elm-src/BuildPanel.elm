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
            [ h3 [] [ text "Parameters" ]
            , selectBuildMode buildMode
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


simpleOption : String -> Html msg
simpleOption optionValue =
    option [ value optionValue ] [ text optionValue ]


buildPanelStyling : List Css.Mixin
buildPanelStyling =
    [ Css.top (Css.px 60), Css.left (Css.px 30) ]


basicParameterInputForm : ParametersDict -> InputValuesDict -> Html Msg
basicParameterInputForm parametersDict currentInputDict =
    Html.div []
        (createParametersSections currentInputDict
            ++ [ parameterSubmit parametersDict
               , button [ onClick Clear ] [ text "Clear" ]
               ]
        )


advancedParameterInputForm : ParametersDict -> InputValuesDict -> Html Msg
advancedParameterInputForm parametersDict currentInputDict =
    Html.div []
        [ button [ onClick AddChain ] [ text "Add Chain" ]
        , Html.div [ class [ FlexContainerCss ] ]
            (createParametersSections currentInputDict)
        , parameterSubmit parametersDict
        , button [ onClick Clear ] [ text "Clear" ]
        ]


createParametersSections : InputValuesDict -> List (Html Msg)
createParametersSections currentInputDict =
    Dict.toList currentInputDict
        |> List.map chainInputSection


chainInputSection : (SectionID, InputValues) -> Html Msg
chainInputSection (sectionID, currentInput) =
    List.map (parameterInput sectionID) (allParameters currentInput)
        ++ [ sequenceInput sectionID
                ( "Sequence", Sequence, currentInput.sequence, currentInput.register )
           ]
        |> div [ class [ FlexItemCss ] ]


allParameters : InputValues -> List ( String, Parameter, String )
allParameters currentInput =
    [ ( "Oligomer State", OligomerState, currentInput.oligomerState )
    , ( "Radius", Radius, currentInput.radius )
    , ( "Pitch", Pitch, currentInput.pitch )
    , ( "Interface Angle", PhiCA, currentInput.phiCA )
    ]


parameterInput : SectionID -> ( String, Parameter, String ) -> Html Msg
parameterInput sectionID ( parameterLabel, parameter, currentParameter ) =
    div [ class [ ParameterInputCss ] ]
        [ text parameterLabel
        , br [] []
        , input
            [ type_ "text"
            , name parameterLabel
            , placeholder parameterLabel
            , onInput (EditParameter sectionID parameter)
            , styles inputStyling
            , value currentParameter
            ]
            []
        ]


sequenceInput : SectionID -> ( String, Parameter, String, String ) -> Html Msg
sequenceInput sectionID ( parameterLabel, parameter, currentSequence, currentRegister ) =
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
            , onInput (EditParameter sectionID parameter)
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
        [ value currentRegister, onInput (EditParameter sectionID Register) ]
        (List.map simpleOption [ "a", "b", "c", "d", "e", "f", "g" ])


toggleBuildPanel : Html Msg
toggleBuildPanel =
    div
        [ class [ OverlayPanelCss, PanelToggleCss ]
        , onClick (TogglePanel BuildPanel)
        ]
        [ text "Build" ]

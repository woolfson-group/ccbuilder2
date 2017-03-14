module BuildPanel exposing (..)

import BuilderCss exposing (CssClasses(..), cssNamespace, panelStyling)
import Css
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import ParameterValidation
    exposing
        ( containsInvalidParameter
        , editParameterValue
        , invalidParameterDict
        )
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
        (List.map (toString >> simpleOption) (List.range 2 30))


simpleOption : String -> Html msg
simpleOption optionValue =
    option [ value optionValue ] [ text optionValue ]


buildPanelStyling : List Css.Mixin
buildPanelStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 35)
    , Css.overflow Css.auto
    , Css.maxHeight (Css.pct 80)
    ]


basicParameters : InputValues -> List ( String, Parameter, String )
basicParameters currentInput =
    [ ( "Radius", Radius, currentInput.radius )
    , ( "Pitch", Pitch, currentInput.pitch )
    , ( "Interface Angle", PhiCA, currentInput.phiCA )
    ]


advancedParameters : InputValues -> List ( String, Parameter, String )
advancedParameters currentInput =
    [ ( "Super-Helical Rotation", SuperHelicalRotation, currentInput.superHelRot )
    , ( "Z-Shift", ZShift, currentInput.zShift )
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
    List.map
        allParameterInput
        (basicParameters currentInput)
        |> (++)
            [ allSequenceInput
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
    let
        inputChunks =
            Dict.toList currentInputDict
                |> chunks 4
    in
        Html.div []
            [ h3 [] [ text "Parameters" ]
            , Html.div
                []
                (List.map createParametersSections inputChunks)
            , parameterSubmit parametersDict
            , button [ onClick Clear ] [ text "Clear" ]
            ]


chunks : Int -> List a -> List (List a)
chunks k xs =
    let
        len =
            List.length xs
    in
        if len > k then
            List.take k xs :: chunks k (List.drop k xs)
        else
            [ xs ]


createParametersSections : List ( SectionID, InputValues ) -> Html Msg
createParametersSections currentInputChunk =
    List.map singleChainInputSection currentInputChunk
        |> Html.div [ class [ FlexContainerCss ] ]


singleChainInputSection : ( SectionID, InputValues ) -> Html Msg
singleChainInputSection ( sectionID, currentInput ) =
    List.map
        (singleParameterInput sectionID)
        ((basicParameters currentInput) ++ (advancedParameters currentInput))
        ++ [ singleSequenceInput sectionID
                ( "Sequence", Sequence, currentInput.sequence, currentInput.register )
           ]
        ++ [ input
                [ type_ "checkbox"
                , onClick (EditSingleParameter Orientation sectionID "")
                ]
                []
           , text "Anti Parallel"
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
        , disabled (invalidParameterDict parameters)
        ]
        []


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

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
import Tools exposing (chunks)
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
        , HelixType(..)
        , BuildMode(..)
        , Panel(..)
        , emptyInput
        )


{ class, classList, id } =
    Html.CssHelpers.withNamespace cssNamespace


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


buildPanel :
    HelixType
    -> BuildMode
    -> ParametersDict
    -> InputValuesDict
    -> Bool
    -> Bool
    -> Html Msg
buildPanel helixType buildMode parametersDict currentInputDict building visible =
    let
        panelView =
            case buildMode of
                Basic ->
                    basicParameterInputForm helixType

                Advanced ->
                    advancedParameterInputForm helixType
    in
        div
            [ class [ OverlayPanelCss ]
            , id [ BuildPanel ]
            , styles <| panelStyling ++ buildPanelStyling
            , hidden <| not visible
            ]
            ([ h2 [] [ text "Build" ]
             , hr [] []
             , selectHelixType helixType
             , selectBuildMode buildMode
             , br [] []
             ]
                -- Collagen does not require access to the oligomeric state
                ++ (if helixType == Alpha then
                        [ h3 [] [ text "Oligomeric State" ]
                        , selectOligomericState
                            (Dict.toList parametersDict |> List.length)
                        ]
                    else
                        []
                   )
                ++ [ panelView parametersDict currentInputDict
                   , parameterSubmit building parametersDict
                   , button
                        [ class [ CCBButtonCss ]
                        , onClick Clear
                        ]
                        [ text "Clear" ]
                   ]
            )


selectBuildMode : BuildMode -> Html Msg
selectBuildMode currentBuildMode =
    select
        [ value (toString currentBuildMode)
        , onInput ChangeBuildMode
        ]
        (List.map simpleOption [ "Basic", "Advanced" ])


selectHelixType : HelixType -> Html Msg
selectHelixType currentHelixType =
    select
        [ value (toString currentHelixType)
        , onInput ChangeHelixType
        ]
        (List.map simpleOption [ "Alpha", "Collagen" ])


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
    , Css.zIndex (Css.int 2)
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
    ]


basicParameterInputForm : HelixType -> ParametersDict -> InputValuesDict -> Html Msg
basicParameterInputForm helixType parametersDict currentInputDict =
    Html.div []
        [ h3 [] [ text "Parameters" ]
        , Dict.get 1 currentInputDict
            |> Maybe.withDefault emptyInput
            |> allChainInputSection helixType
        ]


allChainInputSection : HelixType -> InputValues -> Html Msg
allChainInputSection helixType currentInput =
    List.map
        allParameterInput
        (basicParameters currentInput)
        ++ [ allSequenceInput
                helixType
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


allSequenceInput : HelixType -> ( String, Parameter, String, String ) -> Html Msg
allSequenceInput helixType ( parameterLabel, parameter, currentSequence, currentRegister ) =
    div [ class [ ParameterInputCss ] ]
        ([ text parameterLabel
         ]
            ++ (case helixType of
                    Alpha ->
                        [ text " (Register: "
                        , allRegisterSelection 1 currentRegister
                        , text ")"
                        ]

                    Collagen ->
                        []
               )
            ++ [ br [] []
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
        )


advancedParameterInputForm : HelixType -> ParametersDict -> InputValuesDict -> Html Msg
advancedParameterInputForm helixType parametersDict currentInputDict =
    let
        inputChunks =
            Dict.toList currentInputDict
                |> chunks 4
    in
        Html.div []
            [ h3 [] [ text "Parameters" ]
            , Html.div
                []
                (List.map (createParametersSections helixType) inputChunks)
            ]


createParametersSections : HelixType -> List ( SectionID, InputValues ) -> Html Msg
createParametersSections helixType currentInputChunk =
    List.map (singleChainInputSection helixType) currentInputChunk
        |> Html.div [ class [ FlexContainerCss ] ]


singleChainInputSection : HelixType -> ( SectionID, InputValues ) -> Html Msg
singleChainInputSection helixType ( sectionID, currentInput ) =
    [ h4 [] [ text ("Chain " ++ toString sectionID) ] ]
        ++ List.map
            (singleParameterInput sectionID)
            ((basicParameters currentInput) ++ (advancedParameters currentInput))
        ++ [ singleZShiftInput
                sectionID
                ( "Z-Shift", ZShift, currentInput.zShift )
                (if currentInput.linkedSuperHelRot == "True" then
                    True
                 else
                    False
                )
           ]
        ++ [ input
                [ type_ "checkbox"
                , onClick (EditSingleParameter Orientation sectionID "")
                , checked
                    (if currentInput.antiParallel == "True" then
                        True
                     else
                        False
                    )
                ]
                []
           , text "Anti Parallel"
           ]
        ++ [ singleSequenceInput helixType
                sectionID
                ( "Sequence", Sequence, currentInput.sequence, currentInput.register )
           ]
        ++ [ button
                [ class [ CCBButtonCss ]
                , onClick (CopyParameters sectionID)
                ]
                [ text "Copy" ]
           , button
                [ class [ CCBButtonCss ]
                , onClick (PasteParameters sectionID)
                ]
                [ text "Paste" ]
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


singleZShiftInput : SectionID -> ( String, Parameter, String ) -> Bool -> Html Msg
singleZShiftInput sectionID ( parameterLabel, parameter, currentParameter ) isChecked =
    div [ class [ ParameterInputCss ] ]
        [ text (parameterLabel ++ " (Link SHR")
        , input
            [ type_ "checkbox"
            , onClick (EditSingleParameter LinkedSuperHelRot sectionID "")
            , checked isChecked
            ]
            []
        , text ")"
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


singleSequenceInput :
    HelixType
    -> SectionID
    -> ( String, Parameter, String, String )
    -> Html Msg
singleSequenceInput helixType sectionID ( parameterLabel, parameter, currentSequence, currentRegister ) =
    div [ class [ ParameterInputCss ] ]
        ([ text parameterLabel
         ]
            ++ (case helixType of
                    Alpha ->
                        [ text " (Register: "
                        , singleRegisterSelection sectionID currentRegister
                        , text ")"
                        ]

                    Collagen ->
                        []
               )
            ++ [ br [] []
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
        )


inputStyling : List Css.Mixin
inputStyling =
    [ Css.width (Css.pct 100) ]


parameterSubmit : Bool -> ParametersDict -> Html Msg
parameterSubmit building parameters =
    button
        [ class [ CCBButtonCss ]
        , onClick Build
        , disabled ((invalidParameterDict parameters) || building)
        ]
        [ text "Submit" ]


singleRegisterSelection : SectionID -> String -> Html Msg
singleRegisterSelection sectionID currentRegister =
    select
        [ value currentRegister, onInput (EditSingleParameter Register sectionID) ]
        (List.map simpleOption [ "a", "b", "c", "d", "e", "f", "g" ])


allRegisterSelection : SectionID -> String -> Html Msg
allRegisterSelection sectionID currentRegister =
    select
        [ value currentRegister, onInput (EditAllParameters Register) ]
        (List.map simpleOption [ "a", "b", "c", "d", "e", "f", "g" ])


toggleBuildPanel : Html Msg
toggleBuildPanel =
    div
        [ class [ OverlayPanelCss, LeftPanelToggleCss ]
        , onClick (TogglePanel BuildPanel)
        ]
        [ text "Build" ]

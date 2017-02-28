module BuildPanel exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ParameterValidation exposing (containsInvalidParameter, editParameterValue)
import Styling exposing (Styling, panelStyling)
import Types
    exposing
        ( ParameterRecord
        , InputValues
        , Parameter(..)
        , Panel(..)
        )


type BuildPanelMsgs msg
    = BuildPanelMsgs
        { edit : Parameter -> String -> msg
        , submit : msg
        , clear : msg
        , setRegister : String -> msg
        , toggle : Panel -> msg
        }


config :
    { edit : Parameter -> String -> msg
    , submit : msg
    , clear : msg
    , setRegister : String -> msg
    , toggle : Panel -> msg
    }
    -> BuildPanelMsgs msg
config { edit, submit, clear, setRegister, toggle } =
    BuildPanelMsgs
        { edit = edit
        , submit = submit
        , clear = clear
        , setRegister = setRegister
        , toggle = toggle
        }


buildPanel : BuildPanelMsgs msg -> ParameterRecord -> InputValues -> Html msg
buildPanel config parameters currentInput =
    div [ class "overlay-panel", id "command-panel", style <| panelStyling ++ buildPanelStyling ]
        [ h3 [] [ text "Parameters" ]
        , parameterInputForm config parameters currentInput
        ]


buildPanelStyling : Styling
buildPanelStyling =
    [ ( "top", "7%" )
    , ( "left", "2%" )
    ]


parameterInputForm : BuildPanelMsgs msg -> ParameterRecord -> InputValues -> Html msg
parameterInputForm config parameters currentInput =
    let
        (BuildPanelMsgs { clear }) =
            config
    in
        List.map (parameterInput config) (allParameters currentInput)
            |> flip (List.append)
                [ sequenceInput config
                    ( "Sequence", Sequence, currentInput.sequence, currentInput.register )
                , parameterSubmit config parameters
                , button [ onClick clear ] [ text "Clear" ]
                ]
            |> Html.div []


allParameters : InputValues -> List ( String, Parameter, String )
allParameters currentInput =
    [ ( "Oligomer State", OligomerState, currentInput.oligomerState )
    , ( "Radius", Radius, currentInput.radius )
    , ( "Pitch", Pitch, currentInput.pitch )
    , ( "Interface Angle", PhiCA, currentInput.phiCA )
    ]


parameterInput : BuildPanelMsgs msg -> ( String, Parameter, String ) -> Html msg
parameterInput config ( parameterLabel, parameter, currentParameter ) =
    let
        (BuildPanelMsgs { edit }) =
            config
    in
        div [ class "parameter-input" ]
            [ text parameterLabel
            , br [] []
            , input
                [ type_ "text"
                , name parameterLabel
                , class "parameter-input-box"
                , parameterInputId parameterLabel
                , placeholder parameterLabel
                , onInput (edit parameter)
                , style inputStyling
                , value currentParameter
                ]
                []
            ]


sequenceInput : BuildPanelMsgs msg -> ( String, Parameter, String, String ) -> Html msg
sequenceInput config ( parameterLabel, parameter, currentSequence, currentRegister ) =
    let
        (BuildPanelMsgs { edit }) =
            config
    in
        div [ class "parameter-input" ]
            [ text parameterLabel
            , text " (Register: "
            , registerSelection config currentRegister
            , text ")"
            , br [] []
            , textarea
                [ name parameterLabel
                , class "parameter-input-box"
                , parameterInputId parameterLabel
                , rows 3
                , cols 30
                , style inputStyling
                , placeholder parameterLabel
                , onInput (edit parameter)
                , value currentSequence
                ]
                []
            ]


inputStyling : Styling
inputStyling =
    [ ( "width", "100%" ) ]


parameterInputId : String -> Html.Attribute msg
parameterInputId parameterLabel =
    String.toLower parameterLabel
        |> String.split " "
        |> String.join "-"
        |> String.append "input-box-"
        |> id


parameterSubmit : BuildPanelMsgs msg -> ParameterRecord -> Html msg
parameterSubmit (BuildPanelMsgs { submit }) parameters =
    input
        [ type_ "submit"
        , value "Submit"
        , onClick submit
        , disabled (containsInvalidParameter parameters)
        ]
        []


registerSelection : BuildPanelMsgs msg -> String -> Html msg
registerSelection (BuildPanelMsgs { setRegister }) currentRegister =
    select
        [ value currentRegister, onInput setRegister ]
        (List.map registerOption [ "a", "b", "c", "d", "e", "f", "g" ])


registerOption : String -> Html msg
registerOption register =
    option [ value register ] [ text register ]


toggleBuildPanel : BuildPanelMsgs msg -> Html msg
toggleBuildPanel (BuildPanelMsgs { toggle }) =
    div
        [ class "overlay-panel panel-toggle"
        , id "toggle-command-panel"
        , onClick (toggle BuildPanel)
        ]
        [ text "Build" ]

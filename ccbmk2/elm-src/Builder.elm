port module Builder exposing (..)

import BuildPanel
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (field, string, float, decodeString)
import Json.Encode
import Keyboard
import ParameterValidation exposing (containsInvalidParameter, editParameterValue)
import Styling exposing (Styling, panelStyling)
import Task
import Types
    exposing
        ( ParameterRecord
        , InputValues
        , Parameter(..)
        , Panel(..)
        )


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, initialiseViewer () )



-- Model


type alias Model =
    { parameters : ParameterRecord
    , currentInput : InputValues
    , pdbFile : Maybe String
    , score : Maybe Float
    , building : Bool
    , modelHistory : List ParameterRecord
    , panelVisibility : PanelVisibility
    }


type alias PanelVisibility =
    { buildPanel : Bool
    , examplesPanel : Bool
    , buildHistoryPanel : Bool
    }


emptyModel : Model
emptyModel =
    Model emptyParameters emptyInput Nothing Nothing False [] defaultVisibility


emptyParameters : ParameterRecord
emptyParameters =
    ParameterRecord Nothing Nothing Nothing Nothing Nothing "a"


emptyInput : InputValues
emptyInput =
    InputValues "" "" "" "" "" "a"


defaultVisibility : PanelVisibility
defaultVisibility =
    PanelVisibility True False False



-- Ports


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg



-- Update


type Msg
    = EditParameter Parameter String
    | SetRegister String
    | Build
    | ProcessModel (Result Http.Error ( String, Float ))
    | Clear
    | SetParametersAndBuild ParameterRecord
    | KeyMsg Keyboard.KeyCode
    | TogglePanel Panel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditParameter parameter newValue ->
            let
                ( p, i ) =
                    editParameterValue model.parameters model.currentInput parameter newValue
            in
                { model | parameters = p, currentInput = i } ! []

        SetRegister register ->
            let
                oldParameters =
                    model.parameters

                newParameters =
                    { oldParameters | register = register }

                oldInput =
                    model.currentInput

                newInput =
                    { oldInput | register = register }
            in
                { model | parameters = newParameters, currentInput = newInput } ! []

        Build ->
            ( { model | building = True }, sendBuildCmd model.parameters )

        ProcessModel (Ok ( pdbFile, score )) ->
            let
                oldHistory =
                    if List.length model.modelHistory == 10 then
                        List.take 9 model.modelHistory
                    else
                        model.modelHistory

                modelHistory =
                    model.parameters :: oldHistory
            in
                { model
                    | pdbFile = Just pdbFile
                    , score = Just score
                    , building = False
                    , modelHistory = modelHistory
                }
                    ! [ showStructure pdbFile ]

        ProcessModel (Err _) ->
            { model | building = False } ! []

        Clear ->
            { model | parameters = emptyParameters, currentInput = emptyInput } ! []

        SetParametersAndBuild parameters ->
            let
                newInputValues =
                    parametersToInput parameters
            in
                if containsInvalidParameter parameters then
                    model ! []
                else
                    { model | parameters = parameters, currentInput = newInputValues }
                        ! [ Task.perform identity (Task.succeed Build) ]

        KeyMsg keyCode ->
            case keyCode of
                13 ->
                    if containsInvalidParameter model.parameters then
                        model ! []
                    else
                        model ! [ Task.perform identity (Task.succeed Build) ]

                _ ->
                    model ! []

        TogglePanel panel ->
            let
                oldPanelVisibility =
                    model.panelVisibility
            in
                case panel of
                    BuildPanel ->
                        let
                            newPanelVisibility =
                                { oldPanelVisibility
                                    | buildPanel = not oldPanelVisibility.buildPanel
                                    , examplesPanel = False
                                }
                        in
                            { model | panelVisibility = newPanelVisibility } ! []

                    ExamplesPanel ->
                        let
                            newPanelVisibility =
                                { oldPanelVisibility
                                    | buildPanel = False
                                    , examplesPanel = not oldPanelVisibility.examplesPanel
                                }
                        in
                            { model | panelVisibility = newPanelVisibility } ! []

                    BuildHistoryPanel ->
                        let
                            newPanelVisibility =
                                { oldPanelVisibility
                                    | buildHistoryPanel = not oldPanelVisibility.buildHistoryPanel
                                }
                        in
                            { model | panelVisibility = newPanelVisibility } ! []


sendBuildCmd : ParameterRecord -> Cmd Msg
sendBuildCmd parameters =
    Http.send ProcessModel <|
        Http.post
            "/builder/build_model"
            (Http.jsonBody <| parametersJson parameters)
            modellingResultsDecoder


modellingResultsDecoder : Json.Decode.Decoder ( String, Float )
modellingResultsDecoder =
    Json.Decode.map2 (,) (field "pdb" string) (field "score" float)


parametersJson : ParameterRecord -> Json.Encode.Value
parametersJson parameters =
    Json.Encode.object
        [ ( "Oligomer State", parameters.oligomerState |> Maybe.withDefault 0 |> Json.Encode.int )
        , ( "Radius", parameters.radius |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Pitch", parameters.pitch |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Interface Angle", parameters.phiCA |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Sequence", parameters.sequence |> Maybe.withDefault "" |> Json.Encode.string )
        , ( "Register", parameters.register |> Json.Encode.string )
        ]


parametersToInput : ParameterRecord -> InputValues
parametersToInput parameters =
    let
        os =
            maybeNumberToString parameters.oligomerState

        rad =
            maybeNumberToString parameters.radius

        pit =
            maybeNumberToString parameters.pitch

        phi =
            maybeNumberToString parameters.phiCA

        seq =
            Maybe.withDefault "" parameters.sequence

        reg =
            parameters.register
    in
        InputValues os rad pit phi seq reg


maybeNumberToString : Maybe number -> String
maybeNumberToString mNum =
    case mNum of
        Just num ->
            toString num

        Nothing ->
            ""



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyMsg



-- View


view : Model -> Html Msg
view model =
    div [ id "viewer", style viewerStyling ] [ overlayPanels model ]


viewerStyling : Styling
viewerStyling =
    [ ( "position", "fixed" )
    , ( "bottom", "0px" )
    , ( "top", "0px" )
    , ( "left", "0px" )
    , ( "right", "0px" )
    ]


overlayPanels : Model -> Html Msg
overlayPanels model =
    let
        defaultDivs =
            [ siteHeader
            , topLeftToggles
            , topRightToggles
            , buildingStatusPanel model
            , modelInfoPanel model
            ]

        optionalDivs =
            [ ( model.panelVisibility.buildPanel
              , BuildPanel.buildPanel buildConfig model.parameters model.currentInput
              )
            , ( model.panelVisibility.examplesPanel, examplesPanel )
            , ( model.panelVisibility.buildHistoryPanel, buildHistoryPanel model.modelHistory )
            ]

        activeDivs =
            List.filter (\opt -> Tuple.first opt) optionalDivs
                |> List.unzip
                |> Tuple.second

        allDivs =
            defaultDivs ++ activeDivs
    in
        div [ id "overlay-panels" ] allDivs


buildConfig : BuildPanel.BuildPanelMsgs Msg
buildConfig =
    BuildPanel.config
        { edit = EditParameter
        , submit = Build
        , clear = Clear
        , setRegister = SetRegister
        , toggle = TogglePanel
        }


siteHeader : Html msg
siteHeader =
    div [ id "app-header", style <| headerStyling ++ panelStyling ]
        [ header [] [ h1 [] [ text "CCBuilder Mk.2" ] ] ]


headerStyling : Styling
headerStyling =
    [ ( "position", "absolute" )
    , ( "line-height", "50px" )
    , ( "top", "0%" )
    , ( "left", "0%" )
    , ( "width", "100%" )
    ]


topLeftToggles : Html Msg
topLeftToggles =
    div [ id "top-left-toggles", style topLeftTogglesStyling ]
        [ BuildPanel.toggleBuildPanel buildConfig
        , toggleExamplesPanel
        ]


topLeftTogglesStyling : Styling
topLeftTogglesStyling =
    [ ( "top", "7%" )
    , ( "left", "-5px" )
    , ( "z-index", "2" )
    , ( "position", "absolute" )
    ]


topRightToggles : Html Msg
topRightToggles =
    div [ id "top-right-toggles", style topRightTogglesStyling ]
        [ toggleBuildHistoryPanel
        ]


topRightTogglesStyling : Styling
topRightTogglesStyling =
    [ ( "top", "7%" )
    , ( "right", "-5px" )
    , ( "z-index", "2" )
    , ( "position", "absolute" )
    ]



-- Examples Panel


examplesPanel : Html Msg
examplesPanel =
    div [ class "overlay-panel", id "examples-panel", style <| panelStyling ++ examplesPanelStyling ]
        [ h3 [] [ text "Examples" ]
        , button
            [ class "example-button"
            , style exampleButtonStyling
            , onClick <| SetParametersAndBuild basisSetDimer
            ]
            [ text "Dimer" ]
        , br [] []
        , button
            [ class "example-button"
            , style exampleButtonStyling
            , onClick <| SetParametersAndBuild basisSetTrimer
            ]
            [ text "Trimer" ]
        , br [] []
        , button
            [ class "example-button"
            , style exampleButtonStyling
            , onClick <| SetParametersAndBuild basisSetTetramer
            ]
            [ text "Tetramer" ]
        ]


examplesPanelStyling : Styling
examplesPanelStyling =
    [ ( "top", "7%" )
    , ( "left", "2%" )
    ]


exampleButtonStyling : Styling
exampleButtonStyling =
    [ ( "width", "80%" )
    ]


basisSetDimer : ParameterRecord
basisSetDimer =
    { oligomerState = Just 2
    , radius = Just 5.1
    , pitch = Just 226
    , phiCA = Just 26.4
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
        [ class "overlay-panel panel-toggle"
        , id "toggle-examples-panel"
        , onClick (TogglePanel ExamplesPanel)
        ]
        [ text "Examples" ]



-- Model Info


modelInfoPanel : Model -> Html Msg
modelInfoPanel model =
    div
        [ class "overlay-panel"
        , id "model-info-panel"
        , style <| panelStyling ++ modelInfoPanelStyling
        ]
        [ h3 [] [ text "Model Information" ]
        , text "BUDE Energy"
        , br [] []
        , Maybe.map (roundToXDecPlaces 1) model.score
            |> Maybe.map toString
            |> Maybe.withDefault ""
            |> \val -> input [ value val, readonly True ] []
        ]


roundToXDecPlaces : Int -> Float -> Float
roundToXDecPlaces precision num =
    let
        scaling =
            10 ^ precision |> toFloat
    in
        round (num * scaling)
            |> toFloat
            |> flip (/) scaling


modelInfoPanelStyling : Styling
modelInfoPanelStyling =
    [ ( "bottom", "2%" )
    , ( "left", "2%" )
    ]



-- Build History


buildHistoryPanel : List ParameterRecord -> Html Msg
buildHistoryPanel modelHistory =
    div
        [ class "overlay-panel"
        , id "build-history-panel"
        , style <| panelStyling ++ buildHistoryPanelStyling
        ]
        [ h3 [] [ text "Build History" ]
        , table [ id "parameter-history-table" ]
            [ modelDetailTableHeader
            , List.map modelParametersAsRow modelHistory |> tbody []
            ]
        ]


modelDetailTableHeader : Html msg
modelDetailTableHeader =
    thead []
        [ tr [ class "parameter-history-header" ]
            [ th [ style [ ( "width", "6em" ) ] ] [ text "Oligomer State" ]
            , th [ style [ ( "width", "6em" ) ] ] [ text "Radius" ]
            , th [ style [ ( "width", "6em" ) ] ] [ text "Pitch" ]
            , th [ style [ ( "width", "6em" ) ] ] [ text "Interface Angle" ]
            , th [] [ text "Sequence" ]
            , th [ style [ ( "width", "6em" ) ] ] [ text "Register" ]
            ]
        ]


modelParametersAsRow : ParameterRecord -> Html Msg
modelParametersAsRow parameters =
    let
        inputParameters =
            parametersToInput parameters
    in
        tr [ class "parameter-history-row", onClick (SetParametersAndBuild parameters) ]
            [ inputParameters.oligomerState |> makeParameterTh
            , inputParameters.radius |> makeParameterTh
            , inputParameters.pitch |> makeParameterTh
            , inputParameters.phiCA |> makeParameterTh
            , inputParameters.sequence |> makeParameterTh
            , inputParameters.register |> makeParameterTh
            ]


makeParameterTh : String -> Html Msg
makeParameterTh pString =
    text pString
        |> List.singleton
        |> th []


buildHistoryPanelStyling : Styling
buildHistoryPanelStyling =
    [ ( "top", "7%" )
    , ( "right", "2%" )
    ]


toggleBuildHistoryPanel : Html Msg
toggleBuildHistoryPanel =
    div
        [ class "overlay-panel panel-toggle"
        , id "toggle-build-history-panel"
        , onClick (TogglePanel BuildHistoryPanel)
        ]
        [ text "Build History" ]



-- Building Status


buildingStatusPanel : Model -> Html msg
buildingStatusPanel model =
    let
        commonAttr =
            [ class "overlay-panel"
            , id "building-status-panel"
            , style <| buildingStatusStyling ++ panelStyling
            ]
    in
        if model.building then
            div commonAttr
                [ text "Building..."
                , img [ src "static/css/infinity.gif", width 80, height 80 ] []
                ]
        else
            div (hidden True :: commonAttr) []


buildingStatusStyling : Styling
buildingStatusStyling =
    [ ( "top", "50%" )
    , ( "left", "50%" )
    , ( "width", "80px" )
    , ( "height", "80px" )
    ]

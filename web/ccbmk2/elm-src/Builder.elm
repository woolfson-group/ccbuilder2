port module Builder exposing (..)

import BuilderCss exposing (CssClasses(..), cssNamespace, panelStyling)
import BuildPanel
import Css
import Dict
import ExamplesPanel
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import Http
import Json.Decode exposing (field, string, float, decodeString)
import Json.Encode
import Keyboard
import ParameterValidation
    exposing
        ( containsInvalidParameter
        , editParameterValue
        , invalidParameterDict
        )
import Process
import String
import Task
import Time
import Types
    exposing
        ( Msg(..)
        , ParameterRecord
        , SectionID
        , HistoryID
        , ParametersDict
        , emptyParameterRecord
        , InputValues
        , InputValuesDict
        , emptyInput
        , ModellingResults
        , OptimisationResults
        , Parameter(..)
        , BuildMode(..)
        , OptStatus(..)
        , optStatusToString
        , stringToOptStatus
        , Panel(..)
        , Representation
        , RepOption(..)
        )


{ class, classList, id } =
    Html.CssHelpers.withNamespace cssNamespace


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


main : Program (Maybe ExportableModel) Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Ports


port setStorage : ExportableModel -> Cmd msg


port initialiseViewer : () -> Cmd msg


port showStructure : ( String, Representation ) -> Cmd msg


port showAxes : () -> Cmd msg


port newRepresentation : Representation -> Cmd msg


port downloadPdb : ( String, String ) -> Cmd msg



-- Model


type alias Model =
    { parameters : ParametersDict
    , currentInput : InputValuesDict
    , parameterClipBoard : Maybe ParameterRecord
    , oligomericState : Int
    , buildMode : BuildMode
    , pdbFile : Maybe String
    , score : Maybe Float
    , residuesPerTurn : Maybe Float
    , building : Bool
    , optimising : Bool
    , optJobs : List ( String, OptStatus )
    , heat : Int
    , modelHistory : Dict.Dict HistoryID ( ParametersDict, Bool, Float )
    , nextHistoryID : HistoryID
    , panelVisibility : PanelVisibility
    , currentRepresentation : Representation
    }


init : Maybe ExportableModel -> ( Model, Cmd Msg )
init storedModel =
    let
        showDefaultModel =
            if storedModel == Nothing then
                True
            else
                False

        model =
            storedModel
                |> Maybe.map exportableToModel
                |> Maybe.withDefault emptyModel
    in
        model
            ! ([ initialiseViewer () ]
                ++ if showDefaultModel then
                    [ toCommand (SetParametersAndBuild ExamplesPanel.basisSetDimer) ]
                   else
                    [ showStructure
                        ( Maybe.withDefault "" model.pdbFile
                        , model.currentRepresentation
                        )
                    ]
              )


type alias PanelVisibility =
    { buildPanel : Bool
    , examplesPanel : Bool
    , optimisePanel : Bool
    , buildHistoryPanel : Bool
    , viewerPanel : Bool
    }


parameterRecordWithDefault : SectionID -> ParametersDict -> ParameterRecord
parameterRecordWithDefault pRID parameters =
    Dict.get pRID parameters
        |> Maybe.withDefault emptyParameterRecord


inputRecordWithDefault : SectionID -> InputValuesDict -> InputValues
inputRecordWithDefault iVID inputValues =
    Dict.get iVID inputValues
        |> Maybe.withDefault emptyInput


emptyModel : Model
emptyModel =
    { parameters = Dict.fromList [ ( 1, emptyParameterRecord ), ( 2, emptyParameterRecord ) ]
    , currentInput = Dict.fromList [ ( 1, emptyInput ), ( 2, emptyInput ) ]
    , parameterClipBoard = Nothing
    , oligomericState = 2
    , buildMode = Basic
    , pdbFile = Nothing
    , score = Nothing
    , residuesPerTurn = Nothing
    , building = False
    , optimising = False
    , optJobs = []
    , heat = 298
    , modelHistory = Dict.empty
    , nextHistoryID = 1
    , panelVisibility = defaultVisibility
    , currentRepresentation = Representation False True True False False
    }


defaultVisibility : PanelVisibility
defaultVisibility =
    PanelVisibility True False False False False


type alias ExportableModel =
    { parameters : List ( SectionID, ParameterRecord )
    , currentInput : List ( SectionID, InputValues )
    , parameterClipBoard : Maybe ParameterRecord
    , oligomericState : Int
    , pdbFile : Maybe String
    , score : Maybe Float
    , residuesPerTurn : Maybe Float
    , building : Bool
    , optimising : Bool
    , optJobs : List ( String, String )
    , heat : Int
    , modelHistory : List ( HistoryID, ( List ( SectionID, ParameterRecord ), Bool, Float ) )
    , nextHistoryID : HistoryID
    , panelVisibility : PanelVisibility
    , currentRepresentation : Representation
    }


modelToExportable : Model -> ExportableModel
modelToExportable model =
    { parameters = Dict.toList model.parameters
    , currentInput = Dict.toList model.currentInput
    , parameterClipBoard = model.parameterClipBoard
    , oligomericState = model.oligomericState
    , pdbFile = model.pdbFile
    , score = model.score
    , residuesPerTurn = model.residuesPerTurn
    , building = False
    , optimising = False
    , optJobs = exportableOptJobs model.optJobs
    , heat = model.heat
    , modelHistory =
        model.modelHistory
            |> Dict.toList
            |> List.map
                (\( hid, ( params, vis, score ) ) ->
                    ( hid
                    , ( Dict.toList params, vis, score )
                    )
                )
    , nextHistoryID = model.nextHistoryID
    , panelVisibility = model.panelVisibility
    , currentRepresentation = model.currentRepresentation
    }


exportableToModel : ExportableModel -> Model
exportableToModel exportableModel =
    { parameters = Dict.fromList exportableModel.parameters
    , currentInput = Dict.fromList exportableModel.currentInput
    , parameterClipBoard = exportableModel.parameterClipBoard
    , oligomericState = exportableModel.oligomericState
    , buildMode = Basic
    , pdbFile = exportableModel.pdbFile
    , score = exportableModel.score
    , residuesPerTurn = exportableModel.residuesPerTurn
    , building = False
    , optimising = False
    , optJobs = modelOptJobs exportableModel.optJobs
    , heat = exportableModel.heat
    , modelHistory =
        exportableModel.modelHistory
            |> List.map
                (\( hid, ( params, vis, score ) ) ->
                    ( hid
                    , ( Dict.fromList params, vis, score )
                    )
                )
            |> Dict.fromList
    , nextHistoryID = exportableModel.nextHistoryID
    , panelVisibility = exportableModel.panelVisibility
    , currentRepresentation = exportableModel.currentRepresentation
    }


exportableOptJobs : List ( String, OptStatus ) -> List ( String, String )
exportableOptJobs optJobs =
    List.map (\( ojid, status ) -> ( ojid, optStatusToString status )) optJobs


modelOptJobs : List ( String, String ) -> List ( String, OptStatus )
modelOptJobs optJobs =
    List.map
        (\( ojid, statusString ) ->
            ( ojid
            , Result.withDefault Failed (stringToOptStatus statusString)
            )
        )
        optJobs



-- Update
-- The Msg union type can be found in Types.elm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditSingleParameter parameter sectionID newValue ->
            let
                params =
                    parameterRecordWithDefault sectionID model.parameters

                input =
                    inputRecordWithDefault sectionID model.currentInput

                ( p, i ) =
                    editParameterValue params input parameter newValue
            in
                { model
                    | parameters = Dict.insert sectionID p model.parameters
                    , currentInput = Dict.insert sectionID i model.currentInput
                }
                    ! []

        EditAllParameters parameter newValue ->
            model
                ! (List.range 1 model.oligomericState
                    |> List.map (EditSingleParameter parameter)
                    |> List.map2 (\v m -> m v) (List.repeat model.oligomericState newValue)
                    |> List.map toCommand
                  )

        CopyParameters sectionID ->
            { model | parameterClipBoard = Dict.get sectionID model.parameters } ! []

        PasteParameters sectionID ->
            let
                pastedParameters =
                    Maybe.withDefault emptyParameterRecord model.parameterClipBoard
            in
                { model
                    | parameters = Dict.insert sectionID pastedParameters model.parameters
                    , currentInput =
                        Dict.insert sectionID
                            (parametersToInput pastedParameters)
                            model.currentInput
                }
                    ! []

        ChangeBuildMode buildMode ->
            let
                newBuildMode =
                    case buildMode of
                        "Basic" ->
                            Basic

                        "Advanced" ->
                            Advanced

                        _ ->
                            model.buildMode
            in
                { model | buildMode = newBuildMode }
                    ! [ Task.perform NoOp (Process.sleep (10 * Time.millisecond)) ]

        Build ->
            let
                panelVisibility =
                    model.panelVisibility
            in
                if not model.building || invalidParameterDict model.parameters then
                    { model
                        | building = True
                        , panelVisibility =
                            PanelVisibility False False False False False
                    }
                        ! [ sendBuildCmd model.parameters ]
                else
                    model ! []

        Optimise ->
            let
                panelVisibility =
                    model.panelVisibility
            in
                if not model.building then
                    { model
                        | panelVisibility =
                            { panelVisibility
                                | buildPanel = False
                                , examplesPanel = False
                            }
                    }
                        ! [ sendOptimiseCmd model.parameters model.heat ]
                else
                    model ! []

        OptimisationSubmitted (Ok optJobID) ->
            { model | optJobs = ( optJobID, Submitted ) :: model.optJobs } ! []

        OptimisationSubmitted (Err optJobID) ->
            model ! []

        CheckOptJobs _ ->
            model
                ! (List.map Tuple.first model.optJobs
                |> List.map checkJobStatus)

        OptJobStatus (Ok ( ojid, status )) ->
            let
                newOptJobs =
                    Dict.fromList model.optJobs
                        |> Dict.insert
                            ojid
                            (Result.withDefault Failed (stringToOptStatus status))
                        |> Dict.toList
            in
                { model | optJobs = newOptJobs } ! []

        OptJobStatus (Err _) ->
            model ! []

        ProcessModel (Ok { pdbFile, score, residuesPerTurn }) ->
            let
                historyLength =
                    10

                oldHistory =
                    if (Dict.toList model.modelHistory |> List.length) == historyLength then
                        Dict.toList model.modelHistory
                            |> List.reverse
                            |> List.take (historyLength - 1)
                            |> List.reverse
                            |> Dict.fromList
                    else
                        model.modelHistory
            in
                { model
                    | currentInput = parametersDictToInputDict model.parameters
                    , pdbFile = Just pdbFile
                    , score = Just score
                    , residuesPerTurn = Just residuesPerTurn
                    , building = False
                    , modelHistory =
                        Dict.insert model.nextHistoryID ( model.parameters, False, score ) oldHistory
                    , nextHistoryID = model.nextHistoryID + 1
                }
                    ! [ showStructure ( pdbFile, model.currentRepresentation )
                      , toCommand StoreModel
                      ]

        ProcessModel (Err _) ->
            { model | building = False } ! []

        SetHeat heat ->
            { model | heat = String.toInt heat |> Result.withDefault 298 } ! []

        ProcessOptimisation (Ok { parameters, modellingResults }) ->
            let
                parametersDict =
                    List.map2 (,)
                        (List.range 1 model.oligomericState)
                        (List.repeat model.oligomericState parameters)
                        |> Dict.fromList
            in
                { model
                    | parameters = parametersDict
                    , currentInput = parametersDictToInputDict parametersDict
                    , optimising = False
                }
                    ! [ toCommand (ProcessModel (Ok modellingResults)) ]

        ProcessOptimisation (Err error) ->
            { model | optimising = False } ! [ toCommand (ProcessModel (Err error)) ]

        SetOligomericState n ->
            let
                oligomericState =
                    String.toInt n
                        |> Result.toMaybe
                        |> Maybe.withDefault 2
            in
                if oligomericState > model.oligomericState then
                    { model
                        | parameters =
                            List.range (model.oligomericState + 1) oligomericState
                                |> List.map
                                    (\k ->
                                        ( k
                                        , parameterRecordWithDefault 1 model.parameters
                                        )
                                    )
                                |> List.append (Dict.toList model.parameters)
                                |> Dict.fromList
                        , currentInput =
                            List.range (model.oligomericState + 1) oligomericState
                                |> List.map
                                    (\k ->
                                        ( k, inputRecordWithDefault 1 model.currentInput )
                                    )
                                |> List.append (Dict.toList model.currentInput)
                                |> Dict.fromList
                        , oligomericState = oligomericState
                    }
                        ! []
                else
                    { model
                        | parameters =
                            Dict.toList model.parameters
                                |> List.take oligomericState
                                |> Dict.fromList
                        , currentInput =
                            Dict.toList model.currentInput
                                |> List.take oligomericState
                                |> Dict.fromList
                        , oligomericState = oligomericState
                    }
                        ! []

        Clear ->
            { model
                | parameters =
                    Dict.keys model.parameters
                        |> List.map (\k -> ( k, emptyParameterRecord ))
                        |> Dict.fromList
                , currentInput =
                    Dict.keys model.currentInput
                        |> List.map (\k -> ( k, emptyInput ))
                        |> Dict.fromList
            }
                ! []

        DownloadPdb ->
            let
                pdbFile =
                    Maybe.withDefault "" model.pdbFile
            in
                model ! [ downloadPdb ( "ccbuilder_model.pdb", pdbFile ) ]

        SetParametersAndBuild parameters ->
            if invalidParameterDict parameters then
                model ! []
            else
                { model
                    | parameters = parameters
                    , currentInput = parametersDictToInputDict parameters
                    , oligomericState = Dict.toList parameters |> List.length
                }
                    ! [ toCommand Build ]

        KeyMsg keyCode ->
            case keyCode of
                13 ->
                    if invalidParameterDict model.parameters then
                        model ! []
                    else
                        model
                            ! [ Process.sleep (1 * Time.millisecond)
                                    |> Task.andThen (always <| Task.succeed Build)
                                    |> Task.perform identity
                              ]

                _ ->
                    model ! []

        TogglePanel panel ->
            { model | panelVisibility = togglePanelVisibility panel model.panelVisibility }
                {--This task is required to allow the DOM to be rendered
                if it isn't included then the drop down menus will show thead
                incorrect option. --}
                !
                    [ Task.perform NoOp (Process.sleep (10 * Time.millisecond)) ]

        ExpandHistory hID ->
            let
                oldEntry =
                    Dict.get hID model.modelHistory
            in
                case oldEntry of
                    Just ( parametersDict, visible, score ) ->
                        { model
                            | modelHistory =
                                Dict.insert hID ( parametersDict, not visible, score ) model.modelHistory
                        }
                            ! []

                    Nothing ->
                        model ! []

        ShowAxes ->
            model ! [ showAxes () ]

        EditRepresentation repOption ->
            let
                newRep =
                    updateRepresentation repOption model.currentRepresentation
            in
                { model | currentRepresentation = newRep } ! [ newRepresentation newRep ]

        StoreModel ->
            model ! [ setStorage <| modelToExportable model ]

        NoOp _ ->
            model ! []


toCommand : Msg -> Cmd Msg
toCommand msg =
    Task.perform identity (Task.succeed msg)



-- Build Cmd


sendBuildCmd : ParametersDict -> Cmd Msg
sendBuildCmd parameters =
    Http.send ProcessModel <|
        Http.post
            "builder/api/v0.1/build/coiled-coil"
            (Json.Encode.object
                [ ( "Parameters", parametersDictToListJson parameters )
                ]
                |> Http.jsonBody
            )
            modellingResultsDecoder


modellingResultsDecoder : Json.Decode.Decoder ModellingResults
modellingResultsDecoder =
    Json.Decode.map4
        ModellingResults
        (field "model_id" string)
        (field "pdb" string)
        (field "score" float)
        (field "mean_rpt_value" float)


parameterRecordJson : ParameterRecord -> Json.Encode.Value
parameterRecordJson parameters =
    Json.Encode.object
        [ ( "Radius", parameters.radius |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Pitch", parameters.pitch |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Interface Angle", parameters.phiCA |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Sequence", parameters.sequence |> Maybe.withDefault "" |> Json.Encode.string )
        , ( "Register", parameters.register |> Json.Encode.string )
        , ( "Super-Helical Rotation"
          , parameters.superHelRot
                |> Maybe.withDefault 0
                |> Json.Encode.float
          )
        , ( "Orientation", parameters.antiParallel |> Json.Encode.bool )
        , ( "Z-Shift", parameters.zShift |> Maybe.withDefault 0 |> Json.Encode.float )
        ]


parametersDictToListJson : ParametersDict -> Json.Encode.Value
parametersDictToListJson parameters =
    Dict.values parameters
        |> List.map parameterRecordJson
        |> Json.Encode.list



-- Optimisation Cmd


optimisationJson : ParametersDict -> Int -> Json.Encode.Value
optimisationJson parameters heat =
    Json.Encode.object
        [ ( "Parameters", parametersDictToListJson parameters )
        , ( "Heat", Json.Encode.int heat )
        ]


sendOptimiseCmd : ParametersDict -> Int -> Cmd Msg
sendOptimiseCmd parameters heat =
    Http.send OptimisationSubmitted <|
        Http.post
            "builder/api/v0.1/optimise/coiled-coil"
            (optimisationJson parameters heat
                |> Http.jsonBody
            )
            Json.Decode.string


checkJobStatus : String -> Cmd Msg
checkJobStatus optJobId =
    Http.send OptJobStatus <|
        Http.get
            ("builder/api/v0.1/optimise/check-job-status?opt-job-id=" ++ optJobId)
            jobStatusDecoder


jobStatusDecoder : Json.Decode.Decoder ( String, String )
jobStatusDecoder =
    Json.Decode.map2
        (,)
        (field "_id" Json.Decode.string)
        (field "status" Json.Decode.string)


optimisationResultsDecoder : Json.Decode.Decoder OptimisationResults
optimisationResultsDecoder =
    Json.Decode.map2
        OptimisationResults
        (field "parameters" basicParameterJsonDecoder)
        (field "model_and_info" modellingResultsDecoder)


basicParameterJsonDecoder : Json.Decode.Decoder ParameterRecord
basicParameterJsonDecoder =
    Json.Decode.map5
        basicParametersToRecord
        (field "radius" float)
        (field "pitch" float)
        (field "radius" float)
        (field "sequence" string)
        (field "register" string)


basicParametersToRecord : Float -> Float -> Float -> String -> String -> ParameterRecord
basicParametersToRecord radius pitch phiCA sequence register =
    { radius = Just radius
    , pitch = Just pitch
    , phiCA = Just phiCA
    , sequence = Just sequence
    , register = register
    , superHelRot = Nothing
    , antiParallel = False
    , zShift = Nothing
    , linkedSuperHelRot = True
    }


parametersDictToInputDict : ParametersDict -> InputValuesDict
parametersDictToInputDict parameters =
    Dict.toList parameters
        |> List.map (\( k, v ) -> ( k, parametersToInput v ))
        |> Dict.fromList


parametersToInput : ParameterRecord -> InputValues
parametersToInput parameterRecord =
    let
        rad =
            maybeNumberToString parameterRecord.radius

        pit =
            maybeNumberToString parameterRecord.pitch

        phi =
            maybeNumberToString parameterRecord.phiCA

        seq =
            Maybe.withDefault "" parameterRecord.sequence

        reg =
            parameterRecord.register

        rot =
            maybeNumberToString parameterRecord.superHelRot

        ant =
            toString parameterRecord.antiParallel

        zsh =
            maybeNumberToString parameterRecord.zShift

        lsh =
            toString parameterRecord.linkedSuperHelRot
    in
        InputValues rad pit phi seq reg rot ant zsh lsh


maybeNumberToString : Maybe number -> String
maybeNumberToString mNum =
    case mNum of
        Just num ->
            toString num

        Nothing ->
            ""


togglePanelVisibility : Panel -> PanelVisibility -> PanelVisibility
togglePanelVisibility panel currentVisibility =
    case panel of
        BuildPanel ->
            { currentVisibility
                | buildPanel = not currentVisibility.buildPanel
                , optimisePanel = False
                , examplesPanel = False
            }

        ExamplesPanel ->
            { currentVisibility
                | buildPanel = False
                , optimisePanel = False
                , examplesPanel = not currentVisibility.examplesPanel
            }

        OptimisePanel ->
            { currentVisibility
                | buildPanel = False
                , optimisePanel = not currentVisibility.optimisePanel
                , examplesPanel = False
            }

        BuildHistoryPanel ->
            { currentVisibility
                | buildHistoryPanel = not currentVisibility.buildHistoryPanel
                , viewerPanel = False
            }

        ViewerPanel ->
            { currentVisibility
                | buildHistoryPanel = False
                , viewerPanel = not currentVisibility.viewerPanel
            }

        _ ->
            currentVisibility



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second CheckOptJobs
        , Keyboard.presses KeyMsg
        ]



-- View


view : Model -> Html Msg
view model =
    div [ id "viewer", styles viewerStyling ] [ overlayPanels model ]


viewerStyling : List Css.Mixin
viewerStyling =
    [ Css.position Css.fixed
    , Css.bottom (Css.px 0)
    , Css.top (Css.px 0)
    , Css.left (Css.px 0)
    , Css.right (Css.px 0)
    ]


overlayPanels : Model -> Html Msg
overlayPanels model =
    let
        panelDivs =
            [ siteHeader
            , topLeftToggles
            , topRightToggles
            , BuildPanel.buildPanel
                model.buildMode
                model.parameters
                model.currentInput
                model.building
                model.panelVisibility.buildPanel
            , optimisePanel model.buildMode
                model.optJobs
                model.panelVisibility.optimisePanel
                model.heat
            , ExamplesPanel.examplesPanel model.building model.panelVisibility.examplesPanel
            , statusPanel model.building model.optimising
            , buildHistoryPanel
                model.modelHistory
                model.building
                model.panelVisibility.buildHistoryPanel
            , viewerPanel model.panelVisibility.viewerPanel
            , modelInfoPanel model
            ]
                ++ (List.length model.optJobs
                        |> List.range 1
                        |> List.map2 optJobStatus model.optJobs
                   )
    in
        div [] panelDivs


siteHeader : Html msg
siteHeader =
    div
        [ class [ FlexContainerCss ]
        , id [ AppHeaderPanel ]
        , styles <| headerStyling ++ panelStyling
        ]
        [ header [ styles [ Css.width (Css.pct 50) ] ] [ h1 [] [ text "CCBuilder Mk.2" ] ]
        , div
            [ styles
                [ Css.width (Css.pct 50)
                , Css.textAlign Css.right
                , Css.paddingRight (Css.px 10)
                ]
            ]
            [ h3
                [ styles
                    [ Css.position Css.absolute
                    , Css.right (Css.px 50)
                    ]
                ]
                [ text "Powered by" ]
            , a
                [ href "https://github.com/woolfson-group/isambard" ]
                [ img
                    [ styles
                        [ Css.height (Css.pct 80)
                        , Css.position Css.absolute
                        , Css.top (Css.pct 10)
                        , Css.right (Css.px 3)
                        , Css.borderRadius (Css.px 3)
                        ]
                    , src "static/images/logo_small.png"
                    ]
                    []
                ]
            ]
        ]


headerStyling : List Css.Mixin
headerStyling =
    [ Css.position Css.absolute
    , Css.lineHeight (Css.px 50)
    , Css.top (Css.px 0)
    , Css.left (Css.px 0)
    , Css.width (Css.pct 100)
    ]


topLeftToggles : Html Msg
topLeftToggles =
    div [ styles topLeftTogglesStyling ]
        [ BuildPanel.toggleBuildPanel
        , ExamplesPanel.toggleExamplesPanel
        , toggleOptimisePanel
        ]


topLeftTogglesStyling : List Css.Mixin
topLeftTogglesStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 0)
    , Css.zIndex (Css.int 2)
    , Css.position Css.absolute
    ]


topRightToggles : Html Msg
topRightToggles =
    div [ styles topRightTogglesStyling ]
        [ toggleBuildHistoryPanel
        , toggleViewerPanel
        ]


topRightTogglesStyling : List Css.Mixin
topRightTogglesStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px 0)
    , Css.zIndex (Css.int 2)
    , Css.position Css.absolute
    , Css.width (Css.px 30)
    ]



-- Optimise Panel


optimisePanel : BuildMode -> List ( String, OptStatus ) -> Bool -> Int -> Html Msg
optimisePanel buildMode optJobs visible heat =
    let
        advancedBuild =
            case buildMode of
                Basic ->
                    False

                Advanced ->
                    True

        optimising =
            if List.length optJobs > 1 then
                True
            else
                False

        disabledOpt =
            advancedBuild || optimising
    in
        div
            [ class [ OverlayPanelCss ]
            , styles <| panelStyling ++ optimisePanelStyling
            , hidden <| not visible
            ]
            [ h3 [] [ text "Optimise Parameters" ]
            , text "Heat"
            , br [] []
            , input
                [ type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "2000"
                , value (toString heat)
                , onInput SetHeat
                ]
                []
            , br [] []
            , button
                [ onClick Optimise, disabled disabledOpt ]
                [ text "Optimise Model" ]
            ]


optimisePanelStyling : List Css.Mixin
optimisePanelStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 35)
    ]


toggleOptimisePanel : Html Msg
toggleOptimisePanel =
    div
        [ class [ OverlayPanelCss, LeftPanelToggleCss ]
        , onClick (TogglePanel OptimisePanel)
        ]
        [ text "Optimise" ]



-- Model Info


modelInfoPanel : Model -> Html Msg
modelInfoPanel model =
    div
        [ class [ OverlayPanelCss ]
        , id [ ModelInfoPanel ]
        , styles <| panelStyling ++ modelInfoPanelStyling
        ]
        [ h3 [] [ text "Model Information" ]
        , text "BUDE Energy"
        , br [] []
        , Maybe.map (roundToXDecPlaces 1) model.score
            |> Maybe.map toString
            |> Maybe.withDefault ""
            |> \val -> input [ value val, readonly True ] []
        , br [] []
        , text "Residues per Turn"
        , br [] []
        , Maybe.map (roundToXDecPlaces 2) model.residuesPerTurn
            |> Maybe.map toString
            |> Maybe.withDefault ""
            |> \val -> input [ value val, readonly True ] []
        , br [] []
        , downloadStructureButton model.pdbFile
        ]


modelInfoPanelStyling : List Css.Mixin
modelInfoPanelStyling =
    [ Css.bottom (Css.px 20)
    , Css.left (Css.px 35)
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


downloadStructureButton : Maybe String -> Html Msg
downloadStructureButton pdbFile =
    let
        deactivated =
            if pdbFile == Nothing then
                True
            else
                False
    in
        button
            [ class [ CCBButtonCss ]
            , onClick DownloadPdb
            , disabled deactivated
            ]
            [ text "Download PDB" ]



-- Build History


buildHistoryPanel : Dict.Dict Int ( ParametersDict, Bool, Float ) -> Bool -> Bool -> Html Msg
buildHistoryPanel modelHistory building visible =
    div
        [ class [ OverlayPanelCss ]
        , id [ BuildHistoryPanel ]
        , styles <| panelStyling ++ buildHistoryPanelStyling
        , hidden <| not visible
        ]
        [ h3 [] [ text "Build History" ]
        , table []
            [ modelDetailTableHeader
            , List.map2 modelParametersAsRow
                (Dict.toList modelHistory |> List.reverse)
                (List.repeat (List.length <| Dict.toList modelHistory) building)
                |> List.concat
                |> tbody []
            ]
        ]


modelDetailTableHeader : Html msg
modelDetailTableHeader =
    thead []
        [ tr []
            [ th [] []
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Radius" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Pitch" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Interface Angle" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Super-Helical Rotation" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Z-Shift" ]
            , th [] [ text "Sequence" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Register" ]
            , th [] [ text "BUDE Score" ]
            , th [] []
            ]
        ]


modelParametersAsRow : ( HistoryID, ( ParametersDict, Bool, Float ) ) -> Bool -> List (Html Msg)
modelParametersAsRow ( hID, ( parameters, visible, score ) ) building =
    let
        foldedRows =
            Dict.values parameters
                |> List.tail
                |> Maybe.withDefault []
                |> List.map (modelFoldedRow score)
    in
        if not visible then
            [ modelHistoryTopRow hID parameters building visible score ]
        else
            (modelHistoryTopRow hID parameters building visible score)
                :: foldedRows


modelHistoryTopRow : HistoryID -> ParametersDict -> Bool -> Bool -> Float -> Html Msg
modelHistoryTopRow hID parameters building visible score =
    let
        topRowParameters =
            Dict.values parameters
                |> List.head
                |> Maybe.withDefault emptyParameterRecord
    in
        tr
            []
            ([ div
                [ onClick (ExpandHistory hID) ]
                [ if (not visible) then
                    text "▶"
                  else
                    text "▼"
                ]
             ]
                ++ List.map makeParameterTh
                    (parametersToRow topRowParameters score)
                ++ [ button
                        [ class [ CCBButtonCss ]
                        , onClick (SetParametersAndBuild parameters)
                        , disabled building
                        ]
                        [ text "Rebuild" ]
                   ]
            )


modelFoldedRow : Float -> ParameterRecord -> Html Msg
modelFoldedRow score parameters =
    tr
        []
        ([ text " ┋"
         ]
            ++ List.map makeParameterTh
                (parametersToRow parameters score)
        )


parametersToRow : ParameterRecord -> Float -> List String
parametersToRow parameters score =
    [ parameters.radius
        |> Maybe.withDefault 0
        |> roundToXDecPlaces 1
        |> toString
    , parameters.pitch
        |> Maybe.withDefault 0
        |> round
        |> toString
    , parameters.phiCA
        |> Maybe.withDefault 0
        |> roundToXDecPlaces 1
        |> toString
    , parameters.superHelRot
        |> Maybe.withDefault 0
        |> roundToXDecPlaces 1
        |> toString
    , parameters.zShift
        |> Maybe.withDefault 0
        |> roundToXDecPlaces 1
        |> toString
    , parameters.sequence |> Maybe.withDefault ""
    , parameters.register
    , toString <| roundToXDecPlaces 2 score
    ]


makeParameterTh : String -> Html Msg
makeParameterTh pString =
    text pString
        |> List.singleton
        |> th []


buildHistoryPanelStyling : List Css.Mixin
buildHistoryPanelStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px 35)
    ]


toggleBuildHistoryPanel : Html Msg
toggleBuildHistoryPanel =
    div
        [ class [ OverlayPanelCss, RightPanelToggleCss ]
        , onClick (TogglePanel BuildHistoryPanel)
        ]
        [ text "Build History" ]



-- Viewer Panel


viewerPanel : Bool -> Html Msg
viewerPanel visible =
    div
        [ class [ OverlayPanelCss ]
        , id [ ViewerPanel ]
        , styles <| panelStyling ++ viewerPanelStyling
        , hidden <| not visible
        ]
        [ h2 [] [ text "Viewer Options" ]
        , hr [] []
        , h3 [] [ text "Representation" ]
        , hr [] []
        , text "Backbone"
        , br [] []
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation Cartoon)
            ]
            [ text "Cartoon" ]
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation Trace)
            ]
            [ text "Trace" ]
        , br [] []
        , text "All Atoms"
        , br [] []
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation BallsAndSticks)
            ]
            [ text "Balls and Sticks" ]
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation Spheres)
            ]
            [ text "Spheres" ]
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation Points)
            ]
            [ text "Dots" ]
        , hr [] []
        , h3 [] [ text "Other Options" ]
        , hr [] []
        , button
            [ class [ CCBButtonCss ]
            , onClick ShowAxes
            ]
            [ text "Axes" ]
        , text " (xyz = rgb)"
        ]


viewerPanelStyling : List Css.Mixin
viewerPanelStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px 35)
    ]


updateRepresentation : RepOption -> Representation -> Representation
updateRepresentation repOption oldRep =
    case repOption of
        -- Backbone representations
        Cartoon ->
            { oldRep
                | cartoon = not oldRep.cartoon
                , trace = False
            }

        Trace ->
            { oldRep
                | cartoon = False
                , trace = not oldRep.trace
            }

        -- All atom representations
        BallsAndSticks ->
            { oldRep
                | ballsAndSticks = not oldRep.ballsAndSticks
                , spheres = False
                , points = False
            }

        Spheres ->
            { oldRep
                | ballsAndSticks = False
                , spheres = not oldRep.spheres
                , points = False
            }

        Points ->
            { oldRep
                | ballsAndSticks = False
                , spheres = False
                , points = not oldRep.points
            }


toggleViewerPanel : Html Msg
toggleViewerPanel =
    div
        [ class [ OverlayPanelCss, RightPanelToggleCss ]
        , onClick (TogglePanel ViewerPanel)
        ]
        [ text "Viewer" ]



-- Building Status


statusPanel : Bool -> Bool -> Html msg
statusPanel building optimising =
    div
        [ class [ OverlayPanelCss ]
        , id [ BuildingStatusPanel ]
        , styles <| buildingStatusStyling ++ panelStyling
        , hidden ((not building) && (not optimising))
        ]
        [ if optimising then
            text "Optimising"
          else
            text "Building"
        , img [ src "static/css/infinity.gif", width 80, height 80 ] []
        ]


buildingStatusStyling : List Css.Mixin
buildingStatusStyling =
    [ Css.top (Css.pct 50)
    , Css.left (Css.pct 50)
    , Css.width (Css.px 80)
    , Css.height (Css.px 80)
    ]



-- Optimisation Job


optJobStatus : ( String, OptStatus ) -> Int -> Html msg
optJobStatus ( optID, status ) position =
    div
        [ class [ OverlayPanelCss ]
        , styles <| optJobStatusStyling position ++ panelStyling
        ]
        [ text optID
        , br [] []
        , text (optStatusToString status)
        ]


optJobStatusStyling : Int -> List Css.Mixin
optJobStatusStyling position =
    [ Css.bottom (Css.px 20)
    , Css.left (Css.px <| toFloat (200 * position))
    ]
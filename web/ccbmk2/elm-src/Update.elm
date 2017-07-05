module Update exposing (..)

import Dict
import Http
import Json.Decode exposing (field, string, float, decodeString)
import Json.Encode
import Model exposing (..)
import ParameterValidation
    exposing
        ( containsInvalidParameter
        , editParameterValue
        , invalidParameterDict
        )
import Ports exposing (..)
import Process
import Task
import Time
import Types
    exposing
        ( Msg(..)
        , ParameterRecord
        , SectionID
        , HistoryID
        , ParametersDict
        , InputValues
        , InputValuesDict
        , ModellingResults
        , OptimisationResults
        , Parameter(..)
        , HelixType(..)
        , BuildMode(..)
        , OptStatus(..)
        , stringToOptStatus
        , Panel(..)
        , PanelVisibility
        , Representation
        , RepOption(..)
        , optStatusToString
        , stringToHelixType
        , emptyInput
        , emptyParameterRecord
        , parameterRecordWithDefault
        , inputRecordWithDefault
        , parametersToInput
        , parametersDictToInputDict
        )


-- Update


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

        ChangeHelixType helixType ->
            let
                oligomericState =
                    3

                newHelixType =
                    case helixType of
                        "Alpha" ->
                            Alpha

                        "Collagen" ->
                            Collagen

                        _ ->
                            model.helixType

                newParameters =
                    if newHelixType /= model.helixType then
                        case newHelixType of
                            Collagen ->
                                if model.oligomericState > 3 then
                                    Dict.toList model.parameters
                                        |> List.take oligomericState
                                        |> Dict.fromList
                                else if model.oligomericState < 3 then
                                    Dict.values model.parameters
                                        |> List.head
                                        |> Maybe.withDefault emptyParameterRecord
                                        |> List.repeat oligomericState
                                        |> List.map2 (,)
                                            (List.range 1 oligomericState)
                                        |> Dict.fromList
                                else
                                    model.parameters

                            Alpha ->
                                model.parameters
                    else
                        model.parameters

                newInput =
                    parametersDictToInputDict newParameters
            in
                { model
                    | helixType = newHelixType
                    , oligomericState = oligomericState
                    , parameters = newParameters
                    , currentInput = newInput
                }
                    ! [ Task.perform NoOp (Process.sleep (10 * Time.millisecond)) ]

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

                newParameters =
                    if newBuildMode /= model.buildMode then
                        case newBuildMode of
                            Basic ->
                                Dict.values model.parameters
                                    |> List.head
                                    |> Maybe.withDefault emptyParameterRecord
                                    |> List.repeat model.oligomericState
                                    |> List.map2 (,) (List.range 1 model.oligomericState)
                                    |> Dict.fromList

                            Advanced ->
                                model.parameters
                    else
                        model.parameters

                newInput =
                    parametersDictToInputDict newParameters
            in
                { model
                    | buildMode = newBuildMode
                    , parameters = newParameters
                    , currentInput = newInput
                }
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
                        ! [ case model.helixType of
                                Alpha ->
                                    sendBuildCmd model.parameters

                                Collagen ->
                                    sendCollagenBuildCmd model.parameters
                          ]
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
            { model | optJobs = ( optJobID, Submitted ) :: model.optJobs }
                ! [ toCommand StoreModel ]

        OptimisationSubmitted (Err optJobID) ->
            model ! []

        CheckOptJobs _ ->
            -- Does not check the status of completed jobs.
            model
                ! (model.optJobs
                    |> List.filter
                        (\( _, status ) ->
                            if status == Complete then
                                False
                            else
                                True
                        )
                    |> List.map Tuple.first
                    |> List.map checkJobStatus
                  )

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

        RetrieveOptimisation optJobId ->
            { model
                | optJobs =
                    List.filter (\( ojid, _ ) -> ojid /= optJobId) model.optJobs
            }
                ! [ retreiveOptimisation optJobId ]

        ProcessModel (Ok { helixType, pdbFile, score, residuesPerTurn }) ->
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
                    | helixType = Result.withDefault Alpha (stringToHelixType helixType)
                    , currentInput = parametersDictToInputDict model.parameters
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

        ProcessOptimisation (Ok { parameters, modellingResults, oligomericState }) ->
            let
                fullParameters =
                    { radius = parameters.radius
                    , pitch = parameters.pitch
                    , phiCA = parameters.phiCA
                    , sequence = parameters.sequence
                    , register = parameters.register
                    , antiParallel = False
                    , linkedSuperHelRot = True
                    , superHelRot = Just 0
                    , zShift = Just 0
                    }

                parametersDict =
                    List.map2 (,)
                        (List.range 1 oligomericState)
                        (List.repeat oligomericState fullParameters)
                        |> Dict.fromList
            in
                { model
                    | parameters = parametersDict
                    , currentInput = parametersDictToInputDict parametersDict
                }
                    ! [ toCommand (ProcessModel (Ok modellingResults)) ]

        ProcessOptimisation (Err error) ->
            model ! [ toCommand (ProcessModel (Err error)) ]

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

        SetParametersAndBuild parameters helixType ->
            if invalidParameterDict parameters then
                model ! []
            else
                { model
                    | helixType = helixType
                    , parameters = parameters
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


sendCollagenBuildCmd : ParametersDict -> Cmd Msg
sendCollagenBuildCmd parameters =
    Http.send ProcessModel <|
        Http.post
            "builder/api/v0.1/build/collagen"
            (Json.Encode.object
                [ ( "Parameters", parametersDictToListJson parameters )
                ]
                |> Http.jsonBody
            )
            modellingResultsDecoder


modellingResultsDecoder : Json.Decode.Decoder ModellingResults
modellingResultsDecoder =
    Json.Decode.map5
        ModellingResults
        (field "model_id" string)
        (field "helix_type" string)
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


retreiveOptimisation : String -> Cmd Msg
retreiveOptimisation optJobId =
    Http.send ProcessOptimisation <|
        Http.get
            ("builder/api/v0.1/optimise/retrieve-opt-job?opt-job-id=" ++ optJobId)
            optimisationResultsDecoder


optimisationResultsDecoder : Json.Decode.Decoder OptimisationResults
optimisationResultsDecoder =
    Json.Decode.map3
        OptimisationResults
        (field "parameters" basicParameterJsonDecoder)
        (field "model_and_info" modellingResultsDecoder)
        (field "oligomeric_state" Json.Decode.int)


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

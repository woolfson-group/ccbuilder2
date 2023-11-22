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
        , helixTypeToString
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
                    editParameterValue params
                        input
                        parameter
                        newValue
                        model.helixType
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
                    {- We need to validate the sequences as O isn't allowed for
                       Alpha type helices.
                    -}
                    ! (Dict.toList newInput
                        |> List.map (\( sid, i ) -> ( sid, i.sequence ))
                        |> List.map
                            (\( sid, seq ) ->
                                EditSingleParameter Sequence sid seq
                            )
                        |> List.map toCommand
                      )

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
                            PanelVisibility False False False False False False
                        , activeInfoBoxes = []
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
                                , optimisePanel = False
                            }
                    }
                        ! [ sendOptimiseCmd model.parameters model.helixType model.heat ]
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

        OptJobStatus in_id (Ok ( out_id, status )) ->
            let
                newOptJobs =
                    Dict.fromList model.optJobs
                        |> Dict.insert
                            out_id
                            (Result.withDefault Failed (stringToOptStatus status))
                        |> Dict.toList
            in
                { model | optJobs = newOptJobs } ! []

        OptJobStatus in_id (Err _) ->
            let
                newOptJobs =
                    Dict.fromList model.optJobs
                        |> Dict.insert in_id Failed
                        |> Dict.toList
            in
                { model | optJobs = newOptJobs } ! []

        RetrieveOptimisation optJobId ->
            { model
                | optJobs =
                    List.filter (\( ojid, _ ) -> ojid /= optJobId) model.optJobs
            }
                ! [ retreiveOptimisation optJobId ]

        ClearOptimisation optJobId ->
            { model
                | optJobs =
                    List.filter (\( ojid, _ ) -> ojid /= optJobId) model.optJobs
            }
                ! []

        ProcessModel (Ok { helixTypeString, pdbFile, score, residuesPerTurn, knobIDs }) ->
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

                helixType =
                    Result.withDefault Alpha (stringToHelixType helixTypeString)
            in
                { model
                    | helixType = helixType
                    , currentInput = parametersDictToInputDict model.parameters
                    , pdbFile = Just pdbFile
                    , score = Just score
                    , residuesPerTurn = Just residuesPerTurn
                    , knobIDs = Just knobIDs
                    , building = False
                    , modelHistory =
                        Dict.insert
                            model.nextHistoryID
                            ( model.parameters, False, score, helixType, Basic, knobIDs )
                            oldHistory
                    , nextHistoryID = model.nextHistoryID + 1
                }
                    ! [ showStructure ( pdbFile, model.currentRepresentation )
                      , toCommand StoreModel
                      ]

        ProcessModel (Err _) ->
            { model | building = False } ! []

        SetHeat heat ->
            { model | heat = String.toInt heat |> Result.withDefault 298 }
                ! []

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

        HighlightKnobs ->
            model ! [ highlightKnobs (Maybe.withDefault [] model.knobIDs) ]

        DownloadPdb ->
            let
                pdbFile =
                    Maybe.withDefault "" model.pdbFile
            in
                model ! [ downloadPdb ( "ccbuilder_model.pdb", pdbFile ) ]

        SetParametersAndBuild parameters helixType buildMode ->
            if invalidParameterDict parameters then
                model ! []
            else
                { model
                    | helixType = helixType
                    , buildMode = buildMode
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
            { model
                | panelVisibility = togglePanelVisibility panel model.panelVisibility
                , activeInfoBoxes = []
            }
                {--This task is required to allow the DOM to be rendered
                if it isn't included then the drop down menus will show thead
                incorrect option. --}
                ! [ Task.perform NoOp (Process.sleep (10 * Time.millisecond)) ]

        ExpandHistory hID ->
            let
                oldEntry =
                    Dict.get hID model.modelHistory
            in
                case oldEntry of
                    Just ( parametersDict, visible, score, helixType, buildMode, knobIDs ) ->
                        { model
                            | modelHistory =
                                Dict.insert
                                    hID
                                    ( parametersDict
                                    , not visible
                                    , score
                                    , helixType
                                    , buildMode
                                    , knobIDs
                                    )
                                    model.modelHistory
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

        ShowInfo infoBoxID ->
            { model
                | activeInfoBoxes =
                    infoBoxID :: model.activeInfoBoxes
            }
                ! []

        CloseInfo infoBoxID ->
            { model
                | activeInfoBoxes =
                    List.filter (\x -> x /= infoBoxID) model.activeInfoBoxes
            }
                ! []

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
            "api/v0.1/build/coiled-coil"
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
            "api/v0.1/build/collagen"
            (Json.Encode.object
                [ ( "Parameters", parametersDictToListJson parameters )
                ]
                |> Http.jsonBody
            )
            modellingResultsDecoder


modellingResultsDecoder : Json.Decode.Decoder ModellingResults
modellingResultsDecoder =
    Json.Decode.map6
        ModellingResults
        (field "model_id" string)
        (field "helix_type" string)
        (field "pdb" string)
        (field "score" float)
        (field "mean_rpt_value" float)
        (field "knob_ids" (Json.Decode.list (Json.Decode.list string)))


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
        , ( "Linked SHR", parameters.linkedSuperHelRot |> Json.Encode.bool )
        ]


parametersDictToListJson : ParametersDict -> Json.Encode.Value
parametersDictToListJson parameters =
    Dict.values parameters
        |> List.map parameterRecordJson
        |> Json.Encode.list



-- Optimisation Cmd


optimisationJson : ParametersDict -> HelixType -> Int -> Json.Encode.Value
optimisationJson parameters helixType heat =
    Json.Encode.object
        [ ( "Parameters", parametersDictToListJson parameters )
        , ( "Helix Type", Json.Encode.string (helixTypeToString helixType) )
        , ( "Heat", Json.Encode.int heat )
        ]


sendOptimiseCmd : ParametersDict -> HelixType -> Int -> Cmd Msg
sendOptimiseCmd parameters helixType heat =
    Http.send OptimisationSubmitted <|
        Http.post
            "api/v0.1/optimise/model"
            (optimisationJson parameters helixType heat
                |> Http.jsonBody
            )
            Json.Decode.string


checkJobStatus : String -> Cmd Msg
checkJobStatus optJobId =
    Http.send (OptJobStatus optJobId) <|
        Http.get
            ("api/v0.1/optimise/check-job-status?opt-job-id=" ++ optJobId)
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
            ("api/v0.1/optimise/retrieve-opt-job?opt-job-id=" ++ optJobId)
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
        (field "phiCA" float)
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

        AboutPanel ->
            { currentVisibility
                | aboutPanel = not currentVisibility.aboutPanel
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

module Types exposing (..)

import Dict
import Keyboard
import Http
import Time


type Msg
    = EditSingleParameter Parameter SectionID String
    | EditAllParameters Parameter String
    | CopyParameters SectionID
    | PasteParameters SectionID
    | ChangeBuildMode String
    | Build
    | Optimise
    | ProcessModel (Result Http.Error ModellingResults)
    | SetHeat String
    | OptimisationSubmitted (Result Http.Error String)
    | CheckOptJobs Time.Time
    | OptJobStatus (Result Http.Error ( String, String ))
    | ProcessOptimisation (Result Http.Error OptimisationResults)
    | SetOligomericState String
    | Clear
    | DownloadPdb
    | SetParametersAndBuild ParametersDict
    | KeyMsg Keyboard.KeyCode
    | TogglePanel Panel
    | ExpandHistory HistoryID
    | ShowAxes
    | EditRepresentation RepOption
    | StoreModel
    | NoOp ()


type alias SectionID =
    Int


type alias HistoryID =
    Int


type alias ParametersDict =
    Dict.Dict SectionID ParameterRecord


type alias ParameterRecord =
    { radius : Maybe Float
    , pitch : Maybe Float
    , phiCA : Maybe Float
    , sequence : Maybe String
    , register : String
    , superHelRot : Maybe Float
    , antiParallel : Bool
    , zShift : Maybe Float
    , linkedSuperHelRot : Bool
    }


type alias InputValuesDict =
    Dict.Dict SectionID InputValues


type alias InputValues =
    { radius : String
    , pitch : String
    , phiCA : String
    , sequence : String
    , register : String
    , superHelRot : String
    , antiParallel : String
    , zShift : String
    , linkedSuperHelRot : String
    }


type alias ModellingResults =
    { model_id : String
    , pdbFile : String
    , score : Float
    , residuesPerTurn : Float
    }


type alias OptimisationResults =
    { parameters : ParameterRecord
    , modellingResults : ModellingResults
    }


type Parameter
    = Radius
    | Pitch
    | PhiCA
    | Sequence
    | Register
    | SuperHelicalRotation
    | Orientation
    | ZShift
    | LinkedSuperHelRot


type BuildMode
    = Basic
    | Advanced


type Panel
    = AppHeaderPanel
    | BuildPanel
    | ExamplesPanel
    | OptimisePanel
    | ModelInfoPanel
    | BuildingStatusPanel
    | BuildHistoryPanel
    | ViewerPanel


type alias PanelVisibility =
    { buildPanel : Bool
    , examplesPanel : Bool
    , optimisePanel : Bool
    , buildHistoryPanel : Bool
    , viewerPanel : Bool
    }


type OptStatus
    = Submitted
    | Queued
    | Running
    | Complete
    | Failed


type alias Representation =
    { cartoon : Bool
    , trace : Bool
    , ballsAndSticks : Bool
    , spheres : Bool
    , points : Bool
    }


type RepOption
    = Cartoon
    | Trace
    | BallsAndSticks
    | Spheres
    | Points


optStatusToString : OptStatus -> String
optStatusToString status =
    case status of
        Submitted ->
            "SUBMITTED"

        Queued ->
            "QUEUED"

        Running ->
            "RUNNING"

        Complete ->
            "COMPLETE"

        Failed ->
            "FAILED"


stringToOptStatus : String -> Result String OptStatus
stringToOptStatus statusString =
    case statusString of
        "SUBMITTED" ->
            Ok Submitted

        "QUEUED" ->
            Ok Queued

        "RUNNING" ->
            Ok Running

        "COMPLETE" ->
            Ok Complete

        "FAILED" ->
            Ok Failed

        _ ->
            Err "String could not be converted to OptStatus."


emptyParameterRecord : ParameterRecord
emptyParameterRecord =
    ParameterRecord Nothing Nothing Nothing Nothing "a" Nothing False Nothing True


emptyInput : InputValues
emptyInput =
    InputValues "" "" "" "" "a" "" "False" "" "True"


parameterRecordWithDefault : SectionID -> ParametersDict -> ParameterRecord
parameterRecordWithDefault pRID parameters =
    Dict.get pRID parameters
        |> Maybe.withDefault emptyParameterRecord


inputRecordWithDefault : SectionID -> InputValuesDict -> InputValues
inputRecordWithDefault iVID inputValues =
    Dict.get iVID inputValues
        |> Maybe.withDefault emptyInput

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
    | ChangeHelixType String
    | ChangeBuildMode String
    | Build
    | Optimise
    | ProcessModel (Result Http.Error ModellingResults)
    | SetHeat String
    | OptimisationSubmitted (Result Http.Error String)
    | CheckOptJobs Time.Time
    | OptJobStatus (Result Http.Error ( String, String ))
    | RetrieveOptimisation String
    | ProcessOptimisation (Result Http.Error OptimisationResults)
    | SetOligomericState String
    | Clear
    | DownloadPdb
    | SetParametersAndBuild ParametersDict HelixType BuildMode
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
    , helixTypeString : String
    , pdbFile : String
    , score : Float
    , residuesPerTurn : Float
    }


type alias OptimisationResults =
    { parameters : ParameterRecord
    , modellingResults : ModellingResults
    , oligomericState : Int
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


type HelixType
    = Alpha
    | Collagen


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


stringToBuildMode : String -> Result String BuildMode
stringToBuildMode statusString =
    case statusString of
        "Basic" ->
            Ok Basic

        "Advanced" ->
            Ok Advanced

        _ ->
            Err "String could not be converted to BuildMode."


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


helixTypeToString : HelixType -> String
helixTypeToString helixType =
    case helixType of
        Alpha ->
            "ALPHA"

        Collagen ->
            "COLLAGEN"


stringToHelixType : String -> Result String HelixType
stringToHelixType helixString =
    case helixString of
        "ALPHA" ->
            Ok Alpha

        "COLLAGEN" ->
            Ok Collagen

        _ ->
            Err "String could not be converted to HelixType."


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


parametersDictToInputDict : ParametersDict -> InputValuesDict
parametersDictToInputDict parameters =
    Dict.toList parameters
        |> List.map (\( k, v ) -> ( k, parametersToInput v ))
        |> Dict.fromList

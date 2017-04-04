module Types exposing (..)

import Dict
import Keyboard
import Http


type Msg
    = EditSingleParameter Parameter SectionID String
    | EditAllParameters Parameter String
    | CopyParameters SectionID
    | PasteParameters SectionID
    | ChangeBuildMode String
    | Build
    | Optimise
    | ProcessModel (Result Http.Error ModellingResults)
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
    | NoOp ()


type alias SectionID = Int


type alias HistoryID = Int


type alias ParametersDict = Dict.Dict SectionID ParameterRecord


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


type alias InputValuesDict = Dict.Dict SectionID InputValues


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
    { pdbFile : String
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
    | BuildingStatusPanel
    | BuildHistoryPanel
    | ViewerPanel


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


emptyParameterRecord : ParameterRecord
emptyParameterRecord =
    ParameterRecord Nothing Nothing Nothing Nothing "a" Nothing False Nothing True


emptyInput : InputValues
emptyInput =
    InputValues "" "" "" "" "a" "" "False" "" "True"
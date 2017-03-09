module Types exposing (..)

import Dict
import Keyboard
import Http


type Msg
    = EditParameter SectionID Parameter String
    | ChangeBuildMode String
    | Build
    | ProcessModel (Result Http.Error ModellingResults)
    | AddChain
    | Clear
    | DownloadPdb
    | SetParametersAndBuild ParameterRecord
    | KeyMsg Keyboard.KeyCode
    | TogglePanel Panel


type alias SectionID = Int


type alias ParametersDict = Dict.Dict SectionID ParameterRecord


type alias ParameterRecord =
    { oligomerState : Maybe Int
    , radius : Maybe Float
    , pitch : Maybe Float
    , phiCA : Maybe Float
    , sequence : Maybe String
    , register : String
    }


type alias InputValuesDict = Dict.Dict SectionID InputValues


type alias InputValues =
    { oligomerState : String
    , radius : String
    , pitch : String
    , phiCA : String
    , sequence : String
    , register : String
    }


type alias ModellingResults =
    { pdbFile : String
    , score : Float
    , residuesPerTurn : Float
    }


type Parameter
    = OligomerState
    | Radius
    | Pitch
    | PhiCA
    | Sequence
    | Register


type BuildMode
    = Basic
    | Advanced


type Panel
    = AppHeaderPanel
    | BuildPanel
    | ExamplesPanel
    | BuildingStatusPanel
    | BuildHistoryPanel


emptyParameterRecord : ParameterRecord
emptyParameterRecord =
    ParameterRecord Nothing Nothing Nothing Nothing Nothing "a"


emptyInput : InputValues
emptyInput =
    InputValues "" "" "" "" "" "a"
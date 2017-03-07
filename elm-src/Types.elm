module Types exposing (..)

import Keyboard
import Http


type Msg
    = EditParameter Parameter String
    | Build
    | ProcessModel (Result Http.Error ModellingResults)
    | DownloadPdb
    | Clear
    | SetParametersAndBuild ParameterRecord
    | KeyMsg Keyboard.KeyCode
    | TogglePanel Panel


type alias PID = Int


type alias ParameterRecord =
    { oligomerState : Maybe Int
    , radius : Maybe Float
    , pitch : Maybe Float
    , phiCA : Maybe Float
    , sequence : Maybe String
    , register : String
    }


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


type Panel
    = AppHeaderPanel
    | BuildPanel
    | ExamplesPanel
    | BuildingStatusPanel
    | BuildHistoryPanel

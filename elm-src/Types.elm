module Types exposing (..)

import Keyboard
import Http


type Msg
    = EditParameter Parameter String
    | SetRegister String
    | Build
    | ProcessModel (Result Http.Error ( String, Float ))
    | Clear
    | SetParametersAndBuild ParameterRecord
    | KeyMsg Keyboard.KeyCode
    | TogglePanel Panel


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


type Parameter
    = OligomerState
    | Radius
    | Pitch
    | PhiCA
    | Sequence


type Panel
    = AppHeaderPanel
    | BuildPanel
    | ExamplesPanel
    | BuildingStatusPanel
    | BuildHistoryPanel

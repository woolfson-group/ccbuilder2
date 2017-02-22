module Types exposing (..)


type alias ParameterRecord =
    { oligomerState : Maybe Int
    , radius : Maybe Float
    , pitch : Maybe Float
    , phiCA : Maybe Float
    , sequence : Maybe String
    }


type Parameter
    = OligomerState
    | Radius
    | Pitch
    | PhiCA
    | Sequence
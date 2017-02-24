module Types exposing (..)


type alias ParameterRecord =
    { oligomerState : Maybe Int
    , radius : Maybe Float
    , pitch : Maybe Float
    , phiCA : Maybe Float
    , sequence : Maybe String
    }


type alias InputValues =
    { oligomerState : String
    , radius : String
    , pitch : String
    , phiCA : String
    , sequence : String
    }


type Parameter
    = OligomerState
    | Radius
    | Pitch
    | PhiCA
    | Sequence
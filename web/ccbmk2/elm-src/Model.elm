module Model exposing (..)

import Dict
import Types
    exposing
        ( Msg(..)
        , ParameterRecord
        , SectionID
        , HistoryID
        , HistoryEntry
        , ExpHistoryEntry
        , ParametersDict
        , emptyParameterRecord
        , InputValues
        , InputValuesDict
        , emptyInput
        , ModellingResults
        , KnobIDs
        , OptimisationResults
        , Parameter(..)
        , HelixType(..)
        , BuildMode(..)
        , OptStatus(..)
        , optStatusToString
        , stringToOptStatus
        , Panel(..)
        , PanelVisibility
        , Representation
        , RepOption(..)
        , InfoBox(..)
        , stringToBuildMode
        , stringToHelixType
        , stringToInfoBoxID
        )


-- Each entry in the model history contains:
-- ParametersDict, whether it is expanded, score, HelixType


type alias Model =
    { parameters : ParametersDict
    , currentInput : InputValuesDict
    , parameterClipBoard : Maybe ParameterRecord
    , oligomericState : Int
    , helixType : HelixType
    , buildMode : BuildMode
    , pdbFile : Maybe String
    , score : Maybe Float
    , residuesPerTurn : Maybe Float
    , knobIDs : Maybe KnobIDs
    , building : Bool
    , optJobs : List ( String, OptStatus )
    , heat : Int
    , modelHistory :
        Dict.Dict HistoryID HistoryEntry
    , nextHistoryID : HistoryID
    , panelVisibility : PanelVisibility
    , currentRepresentation : Representation
    , activeInfoBox : Maybe InfoBox
    }


type alias ExportableModel =
    { parameters : List ( SectionID, ParameterRecord )
    , currentInput : List ( SectionID, InputValues )
    , parameterClipBoard : Maybe ParameterRecord
    , oligomericState : Int
    , pdbFile : Maybe String
    , score : Maybe Float
    , residuesPerTurn : Maybe Float
    , knobIDs : Maybe KnobIDs
    , building : Bool
    , optJobs : List ( String, String )
    , heat : Int
    , modelHistory :
        List ( HistoryID, ExpHistoryEntry )
    , nextHistoryID : HistoryID
    , panelVisibility : PanelVisibility
    , currentRepresentation : Representation
    , activeInfoBox : String
    }


emptyModel : Model
emptyModel =
    { parameters = Dict.fromList [ ( 1, emptyParameterRecord ), ( 2, emptyParameterRecord ) ]
    , currentInput = Dict.fromList [ ( 1, emptyInput ), ( 2, emptyInput ) ]
    , parameterClipBoard = Nothing
    , oligomericState = 2
    , helixType = Alpha
    , buildMode = Basic
    , pdbFile = Nothing
    , score = Nothing
    , residuesPerTurn = Nothing
    , knobIDs = Nothing
    , building = False
    , optJobs = []
    , heat = 298
    , modelHistory = Dict.empty
    , nextHistoryID = 1
    , panelVisibility = defaultVisibility
    , currentRepresentation = Representation False True True False False
    , activeInfoBox = Nothing
    }


defaultVisibility : PanelVisibility
defaultVisibility =
    PanelVisibility True False False False False


modelToExportable : Model -> ExportableModel
modelToExportable model =
    { parameters = Dict.toList model.parameters
    , currentInput = Dict.toList model.currentInput
    , parameterClipBoard = model.parameterClipBoard
    , oligomericState = model.oligomericState
    , pdbFile = model.pdbFile
    , score = model.score
    , residuesPerTurn = model.residuesPerTurn
    , knobIDs = model.knobIDs
    , building = False
    , optJobs = exportableOptJobs model.optJobs
    , heat = model.heat
    , modelHistory =
        model.modelHistory
            |> Dict.toList
            |> List.map
                (\( hid, ( params, vis, score, hType, bMode, knobIDs ) ) ->
                    ( hid
                    , ( Dict.toList params, vis, score, toString hType, toString bMode, knobIDs )
                    )
                )
    , nextHistoryID = model.nextHistoryID
    , panelVisibility = model.panelVisibility
    , currentRepresentation = model.currentRepresentation
    , activeInfoBox = toString model.activeInfoBox
    }


exportableToModel : ExportableModel -> Model
exportableToModel exportableModel =
    { parameters = Dict.fromList exportableModel.parameters
    , currentInput = Dict.fromList exportableModel.currentInput
    , parameterClipBoard = exportableModel.parameterClipBoard
    , oligomericState = exportableModel.oligomericState
    , helixType = Alpha
    , buildMode = Basic
    , pdbFile = exportableModel.pdbFile
    , score = exportableModel.score
    , residuesPerTurn = exportableModel.residuesPerTurn
    , knobIDs = exportableModel.knobIDs
    , building = False
    , optJobs = modelOptJobs exportableModel.optJobs
    , heat = exportableModel.heat
    , modelHistory =
        exportableModel.modelHistory
            |> List.map
                (\( hid, ( params, vis, score, hType, bMode, knobIDs ) ) ->
                    ( hid
                    , ( Dict.fromList params
                      , vis
                      , score
                      , Result.withDefault Alpha (stringToHelixType hType)
                      , Result.withDefault Basic (stringToBuildMode bMode)
                      , knobIDs
                      )
                    )
                )
            |> Dict.fromList
    , nextHistoryID = exportableModel.nextHistoryID
    , panelVisibility = exportableModel.panelVisibility
    , currentRepresentation = exportableModel.currentRepresentation
    , activeInfoBox = Result.toMaybe <| stringToInfoBoxID exportableModel.activeInfoBox
    }


exportableOptJobs : List ( String, OptStatus ) -> List ( String, String )
exportableOptJobs optJobs =
    List.map (\( ojid, status ) -> ( ojid, optStatusToString status )) optJobs


modelOptJobs : List ( String, String ) -> List ( String, OptStatus )
modelOptJobs optJobs =
    List.map
        (\( ojid, statusString ) ->
            ( ojid
            , Result.withDefault Failed (stringToOptStatus statusString)
            )
        )
        optJobs

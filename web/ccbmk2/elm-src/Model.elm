module Model exposing (..)

import Dict
import Types
    exposing
        ( Msg(..)
        , ParameterRecord
        , SectionID
        , HistoryID
        , ParametersDict
        , emptyParameterRecord
        , InputValues
        , InputValuesDict
        , emptyInput
        , ModellingResults
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
        )


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
    , building : Bool
    , optJobs : List ( String, OptStatus )
    , heat : Int
    , modelHistory : Dict.Dict HistoryID ( ParametersDict, Bool, Float )
    , nextHistoryID : HistoryID
    , panelVisibility : PanelVisibility
    , currentRepresentation : Representation
    }


type alias ExportableModel =
    { parameters : List ( SectionID, ParameterRecord )
    , currentInput : List ( SectionID, InputValues )
    , parameterClipBoard : Maybe ParameterRecord
    , oligomericState : Int
    , pdbFile : Maybe String
    , score : Maybe Float
    , residuesPerTurn : Maybe Float
    , building : Bool
    , optJobs : List ( String, String )
    , heat : Int
    , modelHistory : List ( HistoryID, ( List ( SectionID, ParameterRecord ), Bool, Float ) )
    , nextHistoryID : HistoryID
    , panelVisibility : PanelVisibility
    , currentRepresentation : Representation
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
    , building = False
    , optJobs = []
    , heat = 298
    , modelHistory = Dict.empty
    , nextHistoryID = 1
    , panelVisibility = defaultVisibility
    , currentRepresentation = Representation False True True False False
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
    , building = False
    , optJobs = exportableOptJobs model.optJobs
    , heat = model.heat
    , modelHistory =
        model.modelHistory
            |> Dict.toList
            |> List.map
                (\( hid, ( params, vis, score ) ) ->
                    ( hid
                    , ( Dict.toList params, vis, score )
                    )
                )
    , nextHistoryID = model.nextHistoryID
    , panelVisibility = model.panelVisibility
    , currentRepresentation = model.currentRepresentation
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
    , building = False
    , optJobs = modelOptJobs exportableModel.optJobs
    , heat = exportableModel.heat
    , modelHistory =
        exportableModel.modelHistory
            |> List.map
                (\( hid, ( params, vis, score ) ) ->
                    ( hid
                    , ( Dict.fromList params, vis, score )
                    )
                )
            |> Dict.fromList
    , nextHistoryID = exportableModel.nextHistoryID
    , panelVisibility = exportableModel.panelVisibility
    , currentRepresentation = exportableModel.currentRepresentation
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

port module Builder exposing (..)

import BuilderCss exposing (CssClasses(..), cssNamespace, panelStyling)
import BuildPanel
import Css
import Dict
import ExamplesPanel
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import Http
import Json.Decode exposing (field, string, float, decodeString)
import Json.Encode
import Keyboard
import ParameterValidation
    exposing
        ( containsInvalidParameter
        , editParameterValue
        , invalidParameterDict
        )
import String
import Task
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
        , Parameter(..)
        , BuildMode(..)
        , Panel(..)
        )


{ class, classList, id } =
    Html.CssHelpers.withNamespace cssNamespace


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    emptyModel
        ! [ initialiseViewer ()
          , msgToCommand (SetParametersAndBuild ExamplesPanel.basisSetDimer)
          ]



-- Model


type alias Model =
    { parameters : ParametersDict
    , currentInput : InputValuesDict
    , parameterClipBoard : Maybe ParameterRecord
    , oligomericState : Int
    , buildMode : BuildMode
    , pdbFile : Maybe String
    , score : Maybe Float
    , residuesPerTurn : Maybe Float
    , building : Bool
    , modelHistory : Dict.Dict HistoryID ( ParametersDict, Bool )
    , nextHistoryID : HistoryID
    , panelVisibility : PanelVisibility
    }


type alias PanelVisibility =
    { buildPanel : Bool
    , examplesPanel : Bool
    , buildHistoryPanel : Bool
    , viewerPanel : Bool
    }


parameterRecordWithDefault : SectionID -> ParametersDict -> ParameterRecord
parameterRecordWithDefault pRID parameters =
    Dict.get pRID parameters
        |> Maybe.withDefault emptyParameterRecord


inputRecordWithDefault : SectionID -> InputValuesDict -> InputValues
inputRecordWithDefault iVID inputValues =
    Dict.get iVID inputValues
        |> Maybe.withDefault emptyInput


emptyModel : Model
emptyModel =
    { parameters = Dict.fromList [ ( 1, emptyParameterRecord ), ( 2, emptyParameterRecord ) ]
    , currentInput = Dict.fromList [ ( 1, emptyInput ), ( 2, emptyInput ) ]
    , parameterClipBoard = Nothing
    , oligomericState = 2
    , buildMode = Basic
    , pdbFile = Nothing
    , score = Nothing
    , residuesPerTurn = Nothing
    , building = False
    , modelHistory = Dict.empty
    , nextHistoryID = 1
    , panelVisibility = defaultVisibility
    }


defaultVisibility : PanelVisibility
defaultVisibility =
    PanelVisibility True False False False



-- Ports


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg


port downloadPdb : ( String, String ) -> Cmd msg



-- Update
-- The Msg union type can be found in Types.elm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditSingleParameter parameter sectionID newValue ->
            let
                params =
                    parameterRecordWithDefault sectionID model.parameters

                input =
                    inputRecordWithDefault sectionID model.currentInput

                ( p, i ) =
                    editParameterValue params input parameter newValue
            in
                { model
                    | parameters = Dict.insert sectionID p model.parameters
                    , currentInput = Dict.insert sectionID i model.currentInput
                }
                    ! []

        EditAllParameters parameter newValue ->
            model
                ! (List.range 1 model.oligomericState
                    |> List.map (EditSingleParameter parameter)
                    |> List.map2 (\v m -> m v) (List.repeat model.oligomericState newValue)
                    |> List.map msgToCommand
                  )

        CopyParameters sectionID ->
            { model | parameterClipBoard = Dict.get sectionID model.parameters } ! []

        PasteParameters sectionID ->
            let
                pastedParameters =
                    Maybe.withDefault emptyParameterRecord model.parameterClipBoard
            in
                { model
                    | parameters = Dict.insert sectionID pastedParameters model.parameters
                    , currentInput =
                        Dict.insert sectionID
                            (parametersToInput pastedParameters)
                            model.currentInput
                }
                    ! []

        ChangeBuildMode buildMode ->
            let
                newBuildMode =
                    case buildMode of
                        "Basic" ->
                            Basic

                        "Advanced" ->
                            Advanced

                        _ ->
                            model.buildMode
            in
                { model | buildMode = newBuildMode } ! []

        Build ->
            let
                panelVisibility = model.panelVisibility
            in
                ( { model
                    | building = True
                    , panelVisibility = 
                        { panelVisibility | buildPanel = False, examplesPanel = False }
                }
                , sendBuildCmd model.parameters
                )

        ProcessModel (Ok { pdbFile, score, residuesPerTurn }) ->
            let
                oldHistory =
                    if (Dict.toList model.modelHistory |> List.length) == 10 then
                        Dict.toList model.modelHistory
                            |> List.take 9
                            |> Dict.fromList
                    else
                        model.modelHistory
            in
                { model
                    | pdbFile = Just pdbFile
                    , score = Just score
                    , residuesPerTurn = Just residuesPerTurn
                    , building = False
                    , modelHistory =
                        Dict.insert model.nextHistoryID ( model.parameters, False ) oldHistory
                    , nextHistoryID = model.nextHistoryID + 1
                }
                    ! [ showStructure pdbFile ]

        ProcessModel (Err _) ->
            { model | building = False } ! []

        SetOligomericState n ->
            let
                oligomericState =
                    String.toInt n
                        |> Result.toMaybe
                        |> Maybe.withDefault 2
            in
                if oligomericState > model.oligomericState then
                    { model
                        | parameters =
                            List.range (model.oligomericState + 1) oligomericState
                                |> List.map (\k -> ( k, emptyParameterRecord ))
                                |> List.append (Dict.toList model.parameters)
                                |> Dict.fromList
                        , currentInput =
                            List.range (model.oligomericState + 1) oligomericState
                                |> List.map (\k -> ( k, emptyInput ))
                                |> List.append (Dict.toList model.currentInput)
                                |> Dict.fromList
                        , oligomericState = oligomericState
                    }
                        ! []
                else
                    { model
                        | parameters =
                            Dict.toList model.parameters
                                |> List.take oligomericState
                                |> Dict.fromList
                        , currentInput =
                            Dict.toList model.currentInput
                                |> List.take oligomericState
                                |> Dict.fromList
                        , oligomericState = oligomericState
                    }
                        ! []

        Clear ->
            { model
                | parameters =
                    Dict.keys model.parameters
                        |> List.map (\k -> ( k, emptyParameterRecord ))
                        |> Dict.fromList
                , currentInput =
                    Dict.keys model.currentInput
                        |> List.map (\k -> ( k, emptyInput ))
                        |> Dict.fromList
            }
                ! []

        DownloadPdb ->
            let
                pdbFile =
                    Maybe.withDefault "" model.pdbFile
            in
                model ! [ downloadPdb ( "ccbuilder_model.pdb", pdbFile ) ]

        SetParametersAndBuild parameters ->
            if invalidParameterDict parameters then
                model ! []
            else
                { model
                    | parameters = parameters
                    , currentInput = parametersDictToInputDict parameters
                    , oligomericState = Dict.toList parameters |> List.length
                }
                    ! [ msgToCommand Build ]

        KeyMsg keyCode ->
            case keyCode of
                13 ->
                    if invalidParameterDict model.parameters then
                        model ! []
                    else
                        model ! [ msgToCommand Build ]

                _ ->
                    model ! []

        TogglePanel panel ->
            { model | panelVisibility = togglePanelVisibility panel model.panelVisibility }
                ! []

        ExpandHistory hID ->
            let
                oldEntry =
                    Dict.get hID model.modelHistory
            in
                case oldEntry of
                    Just ( parametersDict, visible ) ->
                        { model
                            | modelHistory =
                                Dict.insert hID ( parametersDict, not visible ) model.modelHistory
                        }
                            ! []

                    Nothing ->
                        model ! []


msgToCommand : Msg -> Cmd Msg
msgToCommand msg =
    Task.perform identity (Task.succeed msg)


sendBuildCmd : ParametersDict -> Cmd Msg
sendBuildCmd parameters =
    Http.send ProcessModel <|
        Http.post
            "/builder/api/build/coiled-coil"
            (Dict.values parameters
                |> List.map parameterRecordJson
                |> Json.Encode.list
                |> Http.jsonBody
            )
            modellingResultsDecoder


modellingResultsDecoder : Json.Decode.Decoder ModellingResults
modellingResultsDecoder =
    Json.Decode.map3
        ModellingResults
        (field "pdb" string)
        (field "score" float)
        (field "mean_rpt_value" float)


parameterRecordJson : ParameterRecord -> Json.Encode.Value
parameterRecordJson parameters =
    Json.Encode.object
        [ ( "Radius", parameters.radius |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Pitch", parameters.pitch |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Interface Angle", parameters.phiCA |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Sequence", parameters.sequence |> Maybe.withDefault "" |> Json.Encode.string )
        , ( "Register", parameters.register |> Json.Encode.string )
        , ( "Super-Helical Rotation"
          , parameters.superHelRot
                |> Maybe.withDefault 0
                |> Json.Encode.float
          )
        , ( "Orientation", parameters.antiParallel |> Json.Encode.bool )
        , ( "Z-Shift", parameters.zShift |> Maybe.withDefault 0 |> Json.Encode.float )
        ]


parametersDictToInputDict : ParametersDict -> InputValuesDict
parametersDictToInputDict parameters =
    Dict.toList parameters
        |> List.map (\( k, v ) -> ( k, parametersToInput v ))
        |> Dict.fromList


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


togglePanelVisibility : Panel -> PanelVisibility -> PanelVisibility
togglePanelVisibility panel currentVisibility =
    case panel of
        BuildPanel ->
            { currentVisibility
                | buildPanel = not currentVisibility.buildPanel
                , examplesPanel = False
            }

        ExamplesPanel ->
            { currentVisibility
                | buildPanel = False
                , examplesPanel = not currentVisibility.examplesPanel
            }

        BuildHistoryPanel ->
            { currentVisibility
                | buildHistoryPanel = not currentVisibility.buildHistoryPanel
                , viewerPanel = False
            }

        ViewerPanel ->
            { currentVisibility
                | buildHistoryPanel = False
                , viewerPanel = not currentVisibility.viewerPanel
            }
        
        _ ->
            currentVisibility



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyMsg



-- View


view : Model -> Html Msg
view model =
    div [ id "viewer", styles viewerStyling ] [ overlayPanels model ]


viewerStyling : List Css.Mixin
viewerStyling =
    [ Css.position Css.fixed
    , Css.bottom (Css.px 0)
    , Css.top (Css.px 0)
    , Css.left (Css.px 0)
    , Css.right (Css.px 0)
    ]


overlayPanels : Model -> Html Msg
overlayPanels model =
    let
        defaultDivs =
            [ siteHeader
            , topLeftToggles
            , topRightToggles
            , buildingStatusPanel model
            , modelInfoPanel model
            ]

        optionalDivs =
            [ ( model.panelVisibility.buildPanel
              , BuildPanel.buildPanel model.buildMode model.parameters model.currentInput
              )
            , ( model.panelVisibility.examplesPanel, ExamplesPanel.examplesPanel )
            , ( model.panelVisibility.buildHistoryPanel, buildHistoryPanel model.modelHistory )
            ]

        activeDivs =
            List.filter (\opt -> Tuple.first opt) optionalDivs
                |> List.unzip
                |> Tuple.second

        allDivs =
            defaultDivs ++ activeDivs
    in
        div [] allDivs


siteHeader : Html msg
siteHeader =
    div [ id [ AppHeaderPanel ], styles <| headerStyling ++ panelStyling ]
        [ header [] [ h1 [] [ text "CCBuilder Mk.2" ] ] ]


headerStyling : List Css.Mixin
headerStyling =
    [ Css.position Css.absolute
    , Css.lineHeight (Css.px 50)
    , Css.top (Css.px 0)
    , Css.left (Css.px 0)
    , Css.width (Css.pct 100)
    ]


topLeftToggles : Html Msg
topLeftToggles =
    div [ styles topLeftTogglesStyling ]
        [ BuildPanel.toggleBuildPanel
        , ExamplesPanel.toggleExamplesPanel
        ]


topLeftTogglesStyling : List Css.Mixin
topLeftTogglesStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 0)
    , Css.zIndex (Css.int 2)
    , Css.position Css.absolute
    ]


topRightToggles : Html Msg
topRightToggles =
    div [ styles topRightTogglesStyling ]
        [ toggleBuildHistoryPanel
        , toggleViewerPanel
        ]


topRightTogglesStyling : List Css.Mixin
topRightTogglesStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px 0)
    , Css.zIndex (Css.int 2)
    , Css.position Css.absolute
    , Css.width (Css.px 30)
    ]



-- Model Info


modelInfoPanel : Model -> Html Msg
modelInfoPanel model =
    div
        [ class [ OverlayPanelCss ]
        , styles <| panelStyling ++ modelInfoPanelStyling
        ]
        [ h3 [] [ text "Model Information" ]
        , text "BUDE Energy"
        , br [] []
        , Maybe.map (roundToXDecPlaces 1) model.score
            |> Maybe.map toString
            |> Maybe.withDefault ""
            |> \val -> input [ value val, readonly True ] []
        , br [] []
        , text "Residues per Turn"
        , br [] []
        , Maybe.map (roundToXDecPlaces 2) model.residuesPerTurn
            |> Maybe.map toString
            |> Maybe.withDefault ""
            |> \val -> input [ value val, readonly True ] []
        , br [] []
        , downloadStructureButton model.pdbFile
        ]


modelInfoPanelStyling : List Css.Mixin
modelInfoPanelStyling =
    [ Css.bottom (Css.px 20)
    , Css.left (Css.px 35)
    ]


roundToXDecPlaces : Int -> Float -> Float
roundToXDecPlaces precision num =
    let
        scaling =
            10 ^ precision |> toFloat
    in
        round (num * scaling)
            |> toFloat
            |> flip (/) scaling


downloadStructureButton : Maybe String -> Html Msg
downloadStructureButton pdbFile =
    let
        deactivated =
            if pdbFile == Nothing then
                True
            else
                False
    in
        button [ onClick DownloadPdb, disabled deactivated ] [ text "Download PDB" ]



-- Build History


buildHistoryPanel : Dict.Dict Int ( ParametersDict, Bool ) -> Html Msg
buildHistoryPanel modelHistory =
    div
        [ class [ OverlayPanelCss ]
        , id [ BuildHistoryPanel ]
        , styles <| panelStyling ++ buildHistoryPanelStyling
        ]
        [ h3 [] [ text "Build History" ]
        , table []
            [ modelDetailTableHeader
            , List.map modelParametersAsRow (Dict.toList modelHistory |> List.reverse)
                |> List.concat
                |> tbody []
            ]
        ]


modelDetailTableHeader : Html msg
modelDetailTableHeader =
    thead []
        [ tr []
            [ th [] []
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Radius" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Pitch" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Interface Angle" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Super-Helical Rotation" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Z-Shift" ]
            , th [] [ text "Sequence" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Register" ]
            , th [] []
            ]
        ]


modelParametersAsRow : ( HistoryID, ( ParametersDict, Bool ) ) -> List (Html Msg)
modelParametersAsRow ( hID, ( parameters, visible ) ) =
    let
        parameterRecords =
            Dict.values parameters

        topRowParameters =
            List.head parameterRecords
                |> Maybe.withDefault emptyParameterRecord
                |> parametersToInput

        foldedRows =
            List.tail parameterRecords
                |> Maybe.withDefault []
                |> List.map parametersToInput
                |> List.map modelFoldedRow
    in
        if not visible then
            [ modelHistoryTopRow hID parameters topRowParameters visible ]
        else
            (modelHistoryTopRow hID parameters topRowParameters visible)
                :: foldedRows


modelHistoryTopRow : HistoryID -> ParametersDict -> InputValues -> Bool -> Html Msg
modelHistoryTopRow hID parameters inputParameters visible =
    tr
        []
        ([ div
            [ onClick (ExpandHistory hID) ]
            [ if (not visible) then
                text "▶"
              else
                text "▼"
            ]
         ]
            ++ List.map makeParameterTh
                [ inputParameters.radius
                , inputParameters.pitch
                , inputParameters.phiCA
                , inputParameters.superHelRot
                , inputParameters.zShift
                , inputParameters.sequence
                , inputParameters.register
                ]
            ++ [ button [ onClick (SetParametersAndBuild parameters) ] [ text "Rebuild" ] ]
        )


modelFoldedRow : InputValues -> Html Msg
modelFoldedRow inputParameters =
    tr
        []
        ([ text " ┋"
         ]
            ++ List.map makeParameterTh
                [ inputParameters.radius
                , inputParameters.pitch
                , inputParameters.phiCA
                , inputParameters.superHelRot
                , inputParameters.zShift
                , inputParameters.sequence
                , inputParameters.register
                ]
        )


makeParameterTh : String -> Html Msg
makeParameterTh pString =
    text pString
        |> List.singleton
        |> th []


buildHistoryPanelStyling : List Css.Mixin
buildHistoryPanelStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px 35)
    ]


toggleBuildHistoryPanel : Html Msg
toggleBuildHistoryPanel =
    div
        [ class [ OverlayPanelCss, RightPanelToggleCss ]
        , onClick (TogglePanel BuildHistoryPanel)
        ]
        [ text "Build History" ]



-- Viewer Panel

toggleViewerPanel : Html Msg
toggleViewerPanel =
    div
        [ class [ OverlayPanelCss, RightPanelToggleCss ]
        , onClick (TogglePanel ViewerPanel)
        ]
        [ text "Viewer" ]



-- Building Status


buildingStatusPanel : Model -> Html msg
buildingStatusPanel model =
    let
        commonAttr =
            [ class [ OverlayPanelCss ]
            , id [ BuildingStatusPanel ]
            , styles <| buildingStatusStyling ++ panelStyling
            ]
    in
        if model.building then
            div commonAttr
                [ text "Building..."
                , img [ src "static/css/infinity.gif", width 80, height 80 ] []
                ]
        else
            div (hidden True :: commonAttr) []


buildingStatusStyling : List Css.Mixin
buildingStatusStyling =
    [ Css.top (Css.pct 50)
    , Css.left (Css.pct 50)
    , Css.width (Css.px 80)
    , Css.height (Css.px 80)
    ]

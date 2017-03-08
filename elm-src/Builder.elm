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
import ParameterValidation exposing (containsInvalidParameter, editParameterValue)
import Task
import Types
    exposing
        ( Msg(..)
        , ParameterRecord
        , SectionID
        , ParametersDict
        , emptyParameterRecord
        , InputValues
        , InputValuesDict
        , emptyInput
        , ModellingResults
        , Parameter(..)
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
    ( emptyModel, initialiseViewer () )



-- Model


type alias Model =
    { parameters : ParametersDict
    , currentInput : InputValuesDict
    , nextSectionID : SectionID
    , pdbFile : Maybe String
    , score : Maybe Float
    , residuesPerTurn : Maybe Float
    , building : Bool
    , modelHistory : List ParameterRecord
    , panelVisibility : PanelVisibility
    }


type alias PanelVisibility =
    { buildPanel : Bool
    , examplesPanel : Bool
    , buildHistoryPanel : Bool
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
    { parameters = Dict.fromList [ ( 1, emptyParameterRecord ) ]
    , currentInput = Dict.fromList [ ( 1, emptyInput ) ]
    , nextSectionID = 2
    , pdbFile = Nothing
    , score = Nothing
    , residuesPerTurn = Nothing
    , building = False
    , modelHistory = []
    , panelVisibility = defaultVisibility
    }


defaultVisibility : PanelVisibility
defaultVisibility =
    PanelVisibility True False False



-- Ports


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg


port downloadPdb : ( String, String ) -> Cmd msg



-- Update
-- The Msg union type can be found in Types.elm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditParameter parameter newValue ->
            let
                params =
                    parameterRecordWithDefault 1 model.parameters

                input =
                    inputRecordWithDefault 1 model.currentInput

                ( p, i ) =
                    editParameterValue params input parameter newValue
            in
                { model
                    | parameters = Dict.insert 1 p model.parameters
                    , currentInput = Dict.insert 1 i model.currentInput
                }
                    ! []

        Build ->
            ( { model | building = True }
            , sendBuildCmd (parameterRecordWithDefault 1 model.parameters)
            )

        ProcessModel (Ok { pdbFile, score, residuesPerTurn }) ->
            let
                oldHistory =
                    if List.length model.modelHistory == 10 then
                        List.take 9 model.modelHistory
                    else
                        model.modelHistory
            in
                { model
                    | pdbFile = Just pdbFile
                    , score = Just score
                    , residuesPerTurn = Just residuesPerTurn
                    , building = False
                    , modelHistory =
                        parameterRecordWithDefault 1 model.parameters
                            :: oldHistory
                }
                    ! [ showStructure pdbFile ]

        ProcessModel (Err _) ->
            { model | building = False } ! []

        AddChain ->
            { model
                | parameters =
                    Dict.insert model.nextSectionID emptyParameterRecord model.parameters
                , currentInput =
                    Dict.insert model.nextSectionID emptyInput model.currentInput
                , nextSectionID = model.nextSectionID + 1
            }
                ! []

        Clear ->
            { model
                | parameters =
                    Dict.insert 1 emptyParameterRecord model.parameters
                , currentInput = Dict.insert 1 emptyInput model.currentInput
            }
                ! []

        DownloadPdb ->
            let
                pdbFile =
                    Maybe.withDefault "" model.pdbFile
            in
                model ! [ downloadPdb ( "ccbuilder_model.pdb", pdbFile ) ]

        SetParametersAndBuild parameters ->
            if containsInvalidParameter parameters then
                model ! []
            else
                { model
                    | parameters = Dict.insert 1 parameters model.parameters
                    , currentInput =
                        Dict.insert 1 (parametersToInput parameters) model.currentInput
                }
                    ! [ Task.perform identity (Task.succeed Build) ]

        KeyMsg keyCode ->
            case keyCode of
                13 ->
                    if
                        containsInvalidParameter
                            (parameterRecordWithDefault 1 model.parameters)
                    then
                        model ! []
                    else
                        model ! [ Task.perform identity (Task.succeed Build) ]

                _ ->
                    model ! []

        TogglePanel panel ->
            { model | panelVisibility = togglePanelVisibility panel model.panelVisibility }
                ! []


sendBuildCmd : ParameterRecord -> Cmd Msg
sendBuildCmd parameters =
    Http.send ProcessModel <|
        Http.post
            "/builder/api/build/coiled-coil"
            (Http.jsonBody <| parametersJson parameters)
            modellingResultsDecoder


modellingResultsDecoder : Json.Decode.Decoder ModellingResults
modellingResultsDecoder =
    Json.Decode.map3
        ModellingResults
        (field "pdb" string)
        (field "score" float)
        (field "mean_rpt_value" float)


parametersJson : ParameterRecord -> Json.Encode.Value
parametersJson parameters =
    Json.Encode.object
        [ ( "Oligomer State", parameters.oligomerState |> Maybe.withDefault 0 |> Json.Encode.int )
        , ( "Radius", parameters.radius |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Pitch", parameters.pitch |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Interface Angle", parameters.phiCA |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Sequence", parameters.sequence |> Maybe.withDefault "" |> Json.Encode.string )
        , ( "Register", parameters.register |> Json.Encode.string )
        ]


parametersToInput : ParameterRecord -> InputValues
parametersToInput parameters =
    let
        os =
            maybeNumberToString parameters.oligomerState

        rad =
            maybeNumberToString parameters.radius

        pit =
            maybeNumberToString parameters.pitch

        phi =
            maybeNumberToString parameters.phiCA

        seq =
            Maybe.withDefault "" parameters.sequence

        reg =
            parameters.register
    in
        InputValues os rad pit phi seq reg


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
              , BuildPanel.buildPanel model.parameters model.currentInput
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
    , Css.left (Css.px -5)
    , Css.zIndex (Css.int 2)
    , Css.position Css.absolute
    ]


topRightToggles : Html Msg
topRightToggles =
    div [ styles topRightTogglesStyling ]
        [ toggleBuildHistoryPanel
        ]


topRightTogglesStyling : List Css.Mixin
topRightTogglesStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px -5)
    , Css.zIndex (Css.int 2)
    , Css.position Css.absolute
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
    , Css.left (Css.px 30)
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


buildHistoryPanel : List ParameterRecord -> Html Msg
buildHistoryPanel modelHistory =
    div
        [ class [ OverlayPanelCss ]
        , id [ BuildHistoryPanel ]
        , styles <| panelStyling ++ buildHistoryPanelStyling
        ]
        [ h3 [] [ text "Build History" ]
        , table []
            [ modelDetailTableHeader
            , List.map modelParametersAsRow modelHistory |> tbody []
            ]
        ]


modelDetailTableHeader : Html msg
modelDetailTableHeader =
    thead []
        [ tr []
            [ th [ styles [ Css.width (Css.em 6) ] ] [ text "Oligomer State" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Radius" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Pitch" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Interface Angle" ]
            , th [] [ text "Sequence" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Register" ]
            ]
        ]


modelParametersAsRow : ParameterRecord -> Html Msg
modelParametersAsRow parameters =
    let
        inputParameters =
            parametersToInput parameters
    in
        tr [ onClick (SetParametersAndBuild parameters) ]
            [ inputParameters.oligomerState |> makeParameterTh
            , inputParameters.radius |> makeParameterTh
            , inputParameters.pitch |> makeParameterTh
            , inputParameters.phiCA |> makeParameterTh
            , inputParameters.sequence |> makeParameterTh
            , inputParameters.register |> makeParameterTh
            ]


makeParameterTh : String -> Html Msg
makeParameterTh pString =
    text pString
        |> List.singleton
        |> th []


buildHistoryPanelStyling : List Css.Mixin
buildHistoryPanelStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px 30)
    ]


toggleBuildHistoryPanel : Html Msg
toggleBuildHistoryPanel =
    div
        [ class [ OverlayPanelCss, PanelToggleCss ]
        , onClick (TogglePanel BuildHistoryPanel)
        ]
        [ text "Build History" ]



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

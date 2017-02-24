port module Builder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Json.Encode
import ParameterValidation exposing (allParametersValid, editParameterValue)
import Task
import Types
    exposing
        ( ParameterRecord
        , InputValues
        , Parameter(..)
        )


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, initialiseViewer () )



-- Model


type alias Model =
    { parameters : ParameterRecord
    , currentInput : InputValues
    , pdbFile : Maybe String
    , building : Bool
    }


emptyModel : Model
emptyModel =
    Model emptyParameters emptyInput Nothing False


emptyParameters : ParameterRecord
emptyParameters =
    ParameterRecord Nothing Nothing Nothing Nothing Nothing


emptyInput : InputValues
emptyInput =
    InputValues "" "" "" "" ""



-- Ports


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg



-- Update


type Msg
    = EditParameter Parameter String
    | Build
    | ProcessModel (Result Http.Error String)
    | Example ParameterRecord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditParameter parameter newValue ->
            let
                ( p, i ) = editParameterValue model.parameters model.currentInput parameter newValue
            in
                { model | parameters = p, currentInput = i } ! []

        Build ->
            ( { model | building = True }, sendBuildCmd model.parameters )

        ProcessModel (Ok pdbFile) ->
            { model | pdbFile = Just pdbFile, building = False } ! [ showStructure pdbFile ]

        ProcessModel (Err _) ->
            { model | building = False } ! []

        Example parameters ->
            let
                newInputValues = parametersToInput parameters
            in
                { model | parameters = parameters, currentInput = newInputValues } !
                    [ Task.perform identity ( Task.succeed Build ) ]


sendBuildCmd : ParameterRecord -> Cmd Msg
sendBuildCmd parameters =
    Http.send ProcessModel <|
        Http.post
            "/builder/build_model"
            (Http.jsonBody <| parametersJson parameters)
            Json.Decode.string


parametersJson : ParameterRecord -> Json.Encode.Value
parametersJson parameters =
    Json.Encode.object
        [ ( "Oligomer State", parameters.oligomerState |> Maybe.withDefault 0 |> Json.Encode.int )
        , ( "Radius", parameters.radius |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Pitch", parameters.pitch |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Interface Angle", parameters.phiCA |> Maybe.withDefault 0 |> Json.Encode.float )
        , ( "Sequence", parameters.sequence |> Maybe.withDefault "" |> Json.Encode.string )
        ]

parametersToInput : ParameterRecord -> InputValues
parametersToInput parameters =
    let
        os = maybeNumberToString parameters.oligomerState
        rad = maybeNumberToString parameters.radius
        pit = maybeNumberToString parameters.pitch
        phi = maybeNumberToString parameters.phiCA
        seq = Maybe.withDefault "" parameters.sequence
    in
        InputValues os rad pit phi seq


maybeNumberToString : Maybe number -> String
maybeNumberToString mNum =
    case mNum of
        Just num -> toString num
        Nothing -> ""



-- View


type alias Styling =
    List ( String, String )


view : Model -> Html Msg
view model =
    div [ id "viewer", style viewerStyling ]
        [ siteHeader
        , commandPanel model
        , buildingStatusPanel model
        , examplesPanel
        ]


viewerStyling : Styling
viewerStyling =
    [ ( "position", "fixed" )
    , ( "bottom", "0px" )
    , ( "top", "0px" )
    , ( "left", "0px" )
    , ( "right", "0px" )
    ]


siteHeader : Html msg
siteHeader =
    div [ id "app-header", style <| headerStyling ++ panelStyling ]
        [ header [] [ h1 [] [ text "CCBuilder Mk.2" ] ] ]


headerStyling : Styling
headerStyling =
    [ ( "position", "absolute" )
    , ( "line-height", "50px" )
    , ( "top", "0%" )
    , ( "left", "0%" )
    , ( "width", "100%" )
    ]


panelStyling : Styling
panelStyling =
    [ ( "position", "absolute" )
    , ( "z-index", "1" )
    ]



-- Command Panel


commandPanel : Model -> Html Msg
commandPanel model =
    div [ class "overlay-panel", id "command-panel", style <| panelStyling ++ commandPanelStyling ]
        [ h2 [] [ text "Parameters" ]
        , parameterInputForm model
        ]


commandPanelStyling : Styling
commandPanelStyling =
    [ ( "top", "7%" )
    , ( "left", "2%" )
    ]


parameterInputForm : Model -> Html Msg
parameterInputForm model =
    List.map parameterInput ( allParameters model.currentInput )
        |> flip (List.append)
            ([ sequenceInput ( "Sequence", Sequence, model.currentInput.sequence ), parameterSubmit model.parameters ])
        |> Html.div []


allParameters : InputValues -> List ( String, Parameter, String )
allParameters currentInput =
    [ ( "Oligomer State", OligomerState, currentInput.oligomerState )
    , ( "Radius", Radius, currentInput.radius )
    , ( "Pitch", Pitch, currentInput.pitch )
    , ( "Interface Angle", PhiCA, currentInput.phiCA )
    ]


parameterInput : ( String, Parameter, String ) -> Html Msg
parameterInput ( parameterLabel, parameter, currentParameter ) =
    div [ class "parameter-input" ]
        [ text parameterLabel
        , br [] []
        , input
            [ type_ "text"
            , name parameterLabel
            , class "parameter-input-box"
            , parameterInputId parameterLabel
            , placeholder parameterLabel
            , onInput (EditParameter parameter)
            , style inputStyling
            , value currentParameter
            ]
            []
        ]


sequenceInput : ( String, Parameter, String ) -> Html Msg
sequenceInput ( parameterLabel, parameter, currentParameter ) =
    div [ class "parameter-input" ]
        [ text parameterLabel
        , br [] []
        , textarea
            [ name parameterLabel
            , class "parameter-input-box"
            , parameterInputId parameterLabel
            , rows 3
            , cols 30
            , style inputStyling
            , placeholder parameterLabel
            , onInput (EditParameter parameter)
            , value currentParameter
            ]
            []
        ]


inputStyling : Styling
inputStyling =
    [ ( "width", "100%" ) ]


parameterInputId : String -> Html.Attribute msg
parameterInputId parameterLabel =
    String.toLower parameterLabel
        |> String.split " "
        |> String.join "-"
        |> String.append "input-box-"
        |> id


parameterSubmit : ParameterRecord -> Html Msg
parameterSubmit parameters =
    input
        [ type_ "submit"
        , value "Submit"
        , onClick Build
        , disabled (allParametersValid parameters)
        ]
        []



-- Examples Panel


examplesPanel : Html Msg
examplesPanel =
    div [ class "overlay-panel", id "examples-panel", style <| panelStyling ++ examplesPanelStyling ]
        [ h2 [] [ text "Examples" ]
        , button
            [ class "example-button"
            , style exampleButtonStyling
            , onClick <| Example basisSetDimer
            ]
            [ text "Dimer" ]
        , br [] []
        , button
            [ class "example-button"
            , style exampleButtonStyling
            , onClick <| Example basisSetTrimer
            ]
            [ text "Trimer" ]
        , br [] []
        , button
            [ class "example-button"
            , style exampleButtonStyling
            , onClick <| Example basisSetTetramer
            ]
            [ text "Tetramer" ]
        ]


examplesPanelStyling : Styling
examplesPanelStyling =
    [ ( "bottom", "2%" )
    , ( "left", "2%" )
    ]


exampleButtonStyling : Styling
exampleButtonStyling =
    [ ( "width", "80%" )
    ]


basisSetDimer : ParameterRecord
basisSetDimer =
    { oligomerState = Just 2
    , radius = Just 5.1
    , pitch = Just 226
    , phiCA = Just 26.4
    , sequence = Just "EIAALKQEIAALKKENAALKWEIAALKQ"
    }


basisSetTrimer : ParameterRecord
basisSetTrimer =
    { oligomerState = Just 3
    , radius = Just 6.3
    , pitch = Just 194
    , phiCA = Just 20.0
    , sequence = Just "EIAAIKQEIAAIKKEIAAIKWEIAAIKQ"
    }


basisSetTetramer : ParameterRecord
basisSetTetramer =
    { oligomerState = Just 4
    , radius = Just 6.8
    , pitch = Just 213
    , phiCA = Just 22.1
    , sequence = Just "ELAAIKQELAAIKKELAAIKWELAAIKQ"
    }



-- Building Status


buildingStatusPanel : Model -> Html msg
buildingStatusPanel model =
    let
        commonAttr =
            [ class "overlay-panel"
            , id "building-status-panel"
            , style <| buildingStatusStyling ++ panelStyling
            ]
    in
        if model.building then
            div commonAttr
                [ text "Building..."
                , img [ src "static/css/infinity.gif", width 80, height 80 ] []
                ]
        else
            div (hidden True :: commonAttr) []


buildingStatusStyling : Styling
buildingStatusStyling =
    [ ( "top", "50%" )
    , ( "left", "50%" )
    , ( "width", "80px" )
    , ( "height", "80px" )
    ]

port module Builder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Json.Encode
import ParameterValidation exposing (allParametersValid, editParameterValue)
import Types
    exposing
        ( ParameterRecord
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
    , pdbFile : Maybe String
    , building : Bool
    }


emptyModel : Model
emptyModel =
    Model emptyParameters Nothing False


emptyParameters : ParameterRecord
emptyParameters =
    ParameterRecord Nothing Nothing Nothing Nothing Nothing



-- Ports


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg



-- Update


type Msg
    = EditParameter Parameter String
    | Build
    | ProcessModel (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditParameter parameter newValue ->
            { model | parameters = editParameterValue model.parameters parameter newValue } ! []

        Build ->
            ( { model | building = True }, sendBuildCmd model.parameters )

        ProcessModel (Ok pdbFile) ->
            { model | pdbFile = Just pdbFile, building = False } ! [ showStructure pdbFile ]

        ProcessModel (Err _) ->
            { model | building = False } ! []


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



-- View


view : Model -> Html Msg
view model =
    div [ id "viewer", viewerStyle ]
        [ commandPanel model
        , buildingStatusPanel model
        ]


viewerStyle : Html.Attribute msg
viewerStyle =
    style
        [ ( "position", "fixed" )
        , ( "bottom", "0px" )
        , ( "top", "0px" )
        , ( "left", "0px" )
        , ( "right", "0px" )
        ]


commandPanel : Model -> Html Msg
commandPanel model =
    div [ class "overlay-panel", id "command-panel", commandPanelStyle ]
        [ h2 [] [ text "Parameters" ]
        , parameterInputForm model
        ]


commandPanelStyle : Html.Attribute msg
commandPanelStyle =
    style
        [ ( "top", "10px" )
        , ( "left", "10px" )
        ]


parameterInputForm : Model -> Html Msg
parameterInputForm model =
    List.map parameterInput allParameters
        |> flip (List.append)
            ([ sequenceInput ( "Sequence", Sequence ), parameterSubmit model.parameters ])
        |> Html.div []


parameterInput : ( String, Parameter ) -> Html Msg
parameterInput ( parameterLabel, parameter ) =
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
            , inputStyle
            ]
            []
        ]


sequenceInput : ( String, Parameter ) -> Html Msg
sequenceInput ( parameterLabel, parameter ) =
    div [ class "parameter-input" ]
        [ text parameterLabel
        , br [] []
        , textarea
            [ name parameterLabel
            , class "parameter-input-box"
            , parameterInputId parameterLabel
            , rows 3
            , cols 30
            , inputStyle
            , placeholder parameterLabel
            , onInput (EditParameter parameter)
            ]
            []
        ]


inputStyle : Html.Attribute msg
inputStyle = style [ ( "width", "100%" ) ]


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


allParameters : List ( String, Parameter )
allParameters =
    [ ( "Oligomer State", OligomerState )
    , ( "Radius", Radius )
    , ( "Pitch", Pitch )
    , ( "Interface Angle", PhiCA )
    ]


buildingStatusPanel : Model -> Html msg
buildingStatusPanel model =
    let
        commonAttr =
            [ class "overlay-panel", id "building-status-panel", buildingStatusStyle ]
    in
        if model.building then
            div commonAttr
                [ text "Building..."
                , img [ src "static/css/infinity.gif", width 80, height 80 ] []
                ]
        else
            div (hidden True :: commonAttr) []


buildingStatusStyle : Html.Attribute msg
buildingStatusStyle =
    style
        [ ( "top", "50%" )
        , ( "left", "50%" )
        , ( "width", "80px" )
        , ( "height", "80px" )
        ]

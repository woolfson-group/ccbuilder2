module Builder exposing (..)

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
    ( emptyModel, Cmd.none )



-- Model


type alias Model =
    { parameters : ParameterRecord
    , pdbFile : Maybe String
    }


emptyModel : Model
emptyModel =
    Model emptyParameters Nothing


emptyParameters : ParameterRecord
emptyParameters =
    ParameterRecord Nothing Nothing Nothing Nothing Nothing



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
            ( model, sendBuildCmd model.parameters )

        ProcessModel (Ok pdbFile) ->
            { model | pdbFile = Just pdbFile } ! []

        ProcessModel (Err _) ->
            model ! []


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
    div []
        [ parameterInputForm model
        ]


parameterInputForm : Model -> Html Msg
parameterInputForm model =
    List.map parameterInput allParameters
        |> flip (List.append) ([ parameterSubmit model.parameters ])
        |> Html.div []


parameterInput : ( String, Parameter ) -> Html Msg
parameterInput ( parameterLabel, parameter ) =
    input
        [ type_ "text"
        , name parameterLabel
        , placeholder parameterLabel
        , onInput (EditParameter parameter)
        ]
        []


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
    , ( "Sequence", Sequence )
    ]

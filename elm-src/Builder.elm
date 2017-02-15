module Builder exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


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
    { parameters : Parameters
    }


type alias Parameters =
    Dict.Dict ParameterLabel Parameter


type alias ParameterLabel =
    String


type Parameter
    = OligomerState (Maybe Int)
    | Radius (Maybe Float)
    | Pitch (Maybe Float)
    | PhiCA (Maybe Float)
    | Sequence (Maybe String)


emptyModel : Model
emptyModel =
    Dict.fromList
        [ ( "Oligomer State", OligomerState Nothing )
        , ( "Radius", Radius Nothing )
        , ( "Pitch", Pitch Nothing )
        , ( "Interface Angle", PhiCA Nothing )
        , ( "Sequence", Sequence Nothing )
        ]
        |> Model



-- Update


type Msg
    = EditParameter ParameterLabel String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditParameter parameterLabel newValue ->
            let
                newParameters =
                    editParameterValue model.parameters parameterLabel newValue
            in
                { model | parameters = newParameters } ! []


editParameterValue : Parameters -> ParameterLabel -> String -> Parameters
editParameterValue parameters parameterLabel newValue =
    case parameterLabel of
        "Oligomer State" ->
            let
                postValOS =
                    String.toInt newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validateOligomerState
                        |> OligomerState
            in
                Dict.insert parameterLabel postValOS parameters

        "Radius" ->
            let
                postValRadius =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validateRadius
                        |> Radius
            in
                Dict.insert parameterLabel postValRadius parameters

        "Pitch" ->
            let
                postValPitch =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validatePitch
                        |> Pitch
            in
                Dict.insert parameterLabel postValPitch parameters

        "Interface Angle" ->
            let
                postValPhiCA =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validatePhiCA
                        |> PhiCA
            in
                Dict.insert parameterLabel postValPhiCA parameters

        "Sequence" ->
            Dict.insert parameterLabel (validateSequence newValue |> Sequence) parameters

        _ ->
            parameters


validateOligomerState : Int -> Maybe Int
validateOligomerState os =
    if (os > 0) && (isNotNaN <| toFloat os) then
        Just os
    else
        Nothing


validateRadius : Float -> Maybe Float
validateRadius radius =
    if (radius > 0) && (isNotNaN radius) then
        Just radius
    else
        Nothing


validatePitch : Float -> Maybe Float
validatePitch pitch =
    if (pitch > 0) && (isNotNaN pitch) then
        Just pitch
    else
        Nothing


validatePhiCA : Float -> Maybe Float
validatePhiCA phica =
    if isNotNaN phica then
        Just phica
    else
        Nothing


validateSequence : String -> Maybe String
validateSequence sequence =
    let
        allValidChars =
            String.toUpper sequence
                |> String.toList
                |> List.all isAllowedSeqChar
    in
        if allValidChars && (String.length sequence > 0) then
            Just sequence
        else
            Nothing


isAllowedSeqChar : Char -> Bool
isAllowedSeqChar char =
    let
        allowed =
            [ 'A'
            , 'C'
            , 'D'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'I'
            , 'K'
            , 'L'
            , 'M'
            , 'N'
            , 'P'
            , 'Q'
            , 'R'
            , 'S'
            , 'T'
            , 'V'
            , 'W'
            , 'Y'
            ]
    in
        List.member char allowed


isNotNaN : Float -> Bool
isNotNaN =
    not << isNaN



-- View


view : Model -> Html Msg
view model =
    div []
        (List.map parameterInput allParameters)


parameterInput : ParameterLabel -> Html Msg
parameterInput parameterLabel =
    div []
        [ input
            [ type_ "text"
            , placeholder parameterLabel
            , onInput (EditParameter parameterLabel)
            ]
            []
        ]


allParameters : List String
allParameters =
    [ "Oligomer State"
    , "Radius"
    , "Pitch"
    , "Interface Angle"
    , "Sequence"
    ]

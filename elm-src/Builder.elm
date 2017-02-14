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
init = ( Model Dict.empty, Cmd.none )


-- Model


type alias Model =
    { parameters : Parameters
    }

type alias Parameters = Dict.Dict ParameterLabel Parameter

type alias ParameterLabel = String

type Parameter
    = OligomerState (Maybe Int)
    | Radius (Maybe Float)
    | Pitch (Maybe Float)
    | PhiCA (Maybe Float)
    | Sequence (Maybe String)


-- Update


type Msg
    = EditParameter ParameterLabel String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditParameter parameterLabel newValue ->
            let
                newParameters = editParameterValue model.parameters parameterLabel newValue
            in
                { model | parameters = newParameters } ! []


editParameterValue : Parameters -> ParameterLabel -> String -> Parameters
editParameterValue parameters parameterLabel newValue =
    case parameterLabel of
        "Oligomer State" ->
            let
                validatedOS =
                    String.toInt newValue
                    |> Result.toMaybe
                    |> Maybe.andThen validateOligomerState
                    |> OligomerState
                    |> May
            in
                Dict.update parameterLabel validatedOS parameters
        
        "Radius" ->
            let
                maybeRadius =
                    String.toFloat newValue
                    |> Result.toMaybe
            in
                Dict.update parameterLabel (validateRadius maybeRadius) parameters
        
        "Pitch" ->
            let
                validatedPitch =
                    String.toFloat newValue
                    |> Result.toMaybe
                    |> Maybe.andThen validatePitch
                    |> Pitch
            in
                Dict.update parameterLabel validatedPitch parameters
        
        "Interface Angle" ->
            let
                varifiedPhiCA =
                    String.toFloat newValue
                    |> Result.toMaybe
                    |> Maybe.andThen validatePhiCA
                    |> PhiCA
            in
                Dict.update parameterLabel varifiedPhiCA parameters
        
        "Sequence" ->
            Dict.update parameterLabel (Sequence <| validateSequence newValue) parameters


validateOligomerState : Maybe Int -> Maybe OligomerState
validateOligomerState os = 
            if (os > 0) && (isNotNaN <| toFloat os) then 
                Just (OligomerState os)
            else Nothing


validateRadius : Float -> Maybe Float
validateRadius radius = if (radius > 0) && (isNotNaN radius) then Just radius else Nothing


validatePitch : Float -> Maybe Float
validatePitch pitch = if (pitch > 0) && (isNotNaN pitch) then Just pitch else Nothing


validatePhiCA : Float -> Maybe Float
validatePhiCA phica = if isNotNaN phica then Just phica else Nothing


validateSequence : String -> Maybe String
validateSequence sequence =
    let
        allValidChars =
            String.toList sequence
            |> List.all isAllowedSeqChar
    in
        if allValidChars then Just sequence else Nothing        


isAllowedSeqChar : Char -> Bool
isAllowedSeqChar char =
    let
        allowed =
            [ 'A', 'C', 'D', 'E', 'F'
            , 'G', 'H', 'I', 'K', 'L'
            , 'M', 'N', 'P', 'Q', 'R'
            , 'S', 'T', 'V', 'W', 'Y'
            ]
    in
        List.member char allowed

isNotNaN : Float -> Bool
isNotNaN = not << isNaN

-- View


view : Model -> Html Msg
view model =
    div []
        ( List.map parameterInput allParameters )


parameterInput : ParameterLabel -> Html Msg
parameterInput parameterLabel =
    div []
        [ input 
            [ type_ "text"
            , placeholder parameterLabel
            , onInput ( EditParameter parameterLabel )
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
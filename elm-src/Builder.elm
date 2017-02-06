module Builder exposing (..)

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
init = ( emptyModel, Cmd.none )


-- Model


type alias Model =
    { parameters : Parameters
    }

type alias Parameters =
    { oligomerState : Maybe Int
    , radius : Maybe Float
    , pitch : Maybe Float
    , phica : Maybe Float
    , sequence : Maybe String
    }


emptyModel : Model
emptyModel = Model
    { oligomerState = Nothing
    , radius = Nothing
    , pitch = Nothing
    , phica = Nothing
    , sequence = Nothing
    }


-- update


type Msg
    = EditParameter Field String

type Field
    = OligomerState
    | Radius
    | Pitch
    | InterfaceAngle
    | Sequence


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditParameter field newValue ->
            let
                newParameters = editParameterValue model.parameters field newValue
            in
                { model | parameters = newParameters } ! []


editParameterValue : Parameters -> Field -> String -> Parameters
editParameterValue parameters field newValue =
    case field of
        OligomerState ->
            let
                maybeOS =
                    String.toInt newValue
                    |> Result.toMaybe
                    |> Maybe.andThen verifyOligomerState
            in
                { parameters | oligomerState = maybeOS }
        
        Radius ->
            let
                maybeRadius =
                    String.toFloat newValue
                    |> Result.toMaybe
                    |> Maybe.andThen verifyRadius
            in
                { parameters | radius = maybeRadius }
        
        Pitch ->
            let
                maybePitch =
                    String.toFloat newValue
                    |> Result.toMaybe
                    |> Maybe.andThen verifyPitch
            in
                { parameters | pitch = maybePitch }
        
        InterfaceAngle ->
            let
                maybePhiCA =
                    String.toFloat newValue
                    |> Result.toMaybe
                    |> Maybe.andThen verifyPhiCA
            in
                { parameters | phica = maybePhiCA }
        
        Sequence ->
            { parameters | sequence = String.toUpper newValue |> verifySequence }


verifyOligomerState : Int -> Maybe Int
verifyOligomerState os = if (os > 0) && (isNotNaN <| toFloat os) then Just os else Nothing


verifyRadius : Float -> Maybe Float
verifyRadius radius = if (radius > 0) && (isNotNaN radius) then Just radius else Nothing


verifyPitch : Float -> Maybe Float
verifyPitch pitch = if (pitch > 0) && (isNotNaN pitch) then Just pitch else Nothing


verifyPhiCA : Float -> Maybe Float
verifyPhiCA phica = if isNotNaN phica then Just phica else Nothing


verifySequence : String -> Maybe String
verifySequence sequence =
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
        ( List.map parameterInput parameterDetails )


parameterInput : ( String, Field ) -> Html Msg
parameterInput ( placeHolderText, field ) =
    div []
        [ input [ type_ "text", placeholder placeHolderText, onInput ( EditParameter field ) ] []
        ]

parameterDetails : List ( String, Field )
parameterDetails =
    [ ( "Oligomer State", OligomerState )
    , ( "Radius", Radius )
    , ( "Pitch", Pitch )
    , ( "Interface Angle", InterfaceAngle )
    , ( "Sequence", Sequence )
    ]
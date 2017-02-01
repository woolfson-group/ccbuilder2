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
    { oligomerState : Maybe Int
    , radius : Maybe Float
    , pitch : Maybe Float
    , phica : Maybe Float
    , sequence : Maybe String
    }


emptyModel : Model
emptyModel =
    { oligomerState = Nothing
    , radius = Nothing
    , pitch = Nothing
    , phica = Nothing
    , sequence = Nothing
    }


-- update


type Msg
    = OligomerState String
    | Radius String
    | Pitch String
    | InterfaceAngle String
    | Sequence String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OligomerState n ->
            { model | oligomerState = Result.toMaybe ( String.toInt n ) } ! []
        
        Radius r ->
            { model | radius = Result.toMaybe ( String.toFloat r ) } ! []
        
        Pitch p ->
            { model | pitch = Result.toMaybe ( String.toFloat p ) } ! []
        
        InterfaceAngle f ->
            { model | phica = Result.toMaybe ( String.toFloat f ) } ! []
        
        Sequence seq ->
            { model | sequence = Just seq } ! []


-- View


view : Model -> Html Msg
view model =
    div []
        ( List.map parameterInput parameterDetails )


parameterInput : ( String, ( String -> Msg) ) -> Html Msg
parameterInput ( placeHolderText, msg ) =
    div []
        [ input [ type_ "text", placeholder placeHolderText, onInput msg ] []
        ]

parameterDetails : List ( String, ( String -> Msg) )
parameterDetails =
    [ ( "Oligomer State", OligomerState )
    , ( "Radius", Radius )
    , ( "Pitch", Pitch )
    , ( "Interface Angle", InterfaceAngle )
    , ( "Sequence", Sequence )
    ]
module Main exposing (..)

import ExamplesPanel
import Keyboard
import Html exposing (..)
import Model exposing (..)
import Ports exposing (..)
import Time
import Types exposing (Msg(..), HelixType(..), BuildMode(..))
import Update exposing (update, toCommand)
import Views exposing (view)


main : Program (Maybe ExportableModel) Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Maybe ExportableModel -> ( Model, Cmd Msg )
init storedModel =
    let
        showDefaultModel =
            if storedModel == Nothing then
                True
            else
                False

        model =
            storedModel
                |> Maybe.map exportableToModel
                |> Maybe.withDefault emptyModel
    in
        model
            ! ([ initialiseViewer () ]
                ++ if showDefaultModel then
                    [ toCommand (SetParametersAndBuild ExamplesPanel.basisSetDimer Alpha Basic) ]
                   else
                    [ showStructure
                        ( Maybe.withDefault "" model.pdbFile
                        , model.currentRepresentation
                        )
                    ]
              )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (5 * Time.second) CheckOptJobs
        , Keyboard.presses KeyMsg
        ]

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
init _ =
    emptyModel
        ! [ initialiseViewer ()
          , toCommand (SetParametersAndBuild ExamplesPanel.basisSetDimer Alpha Basic)
          ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ Keyboard.presses KeyMsg ]
            ++ if not (List.isEmpty model.optJobs) then
                [ Time.every (5 * Time.second) CheckOptJobs ]
               else
                []
        )

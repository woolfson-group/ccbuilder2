port module Ports exposing (..)

import Model exposing ( ExportableModel )
import Types exposing ( KnobIDs, Representation )

-- Ports


port setStorage : ExportableModel -> Cmd msg


port initialiseViewer : () -> Cmd msg


port showStructure : ( String, Representation ) -> Cmd msg


port showAxes : () -> Cmd msg


port newRepresentation : Representation -> Cmd msg


port highlightKnobs : KnobIDs -> Cmd msg


port downloadPdb : ( String, String ) -> Cmd msg
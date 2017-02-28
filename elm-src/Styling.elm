module Styling exposing (..)


type alias Styling =
    List ( String, String )


panelStyling : Styling
panelStyling =
    [ ( "position", "absolute" )
    , ( "z-index", "1" )
    ]
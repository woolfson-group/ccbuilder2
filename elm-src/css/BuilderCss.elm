module BuilderCss exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Css.Elements exposing (
    body, h1, h2, h3, table, tbody, input, textarea)


cssNamespace : String
cssNamespace = "builder"


type CssClasses
    = OverlayPanel
    | PanelToggle
    | ParameterInput


type CssIds
    = AppHeader
    | Build
    | Examples
    | BuildingStatus
    | ParameterHistory


css : Stylesheet
css =
    ( stylesheet << namespace cssNamespace )
    [ body
        [ color colorPalette.c5
        , margin (px 0)
        , padding (px 0)
        ]
    , h1
        [ margin (px 0)
        , paddingLeft (px 10)
        ]
    , each [ h2, h3 ]
        [ marginTop (px 2)
        , marginBottom (px 2)
        ]
    , Css.Elements.table
        [ borderCollapse collapse
        ]
    , class OverlayPanel
        [ fontFamilies [ "Roboto", "sans-serif" ]
        , backgroundColor colorPalette.c2
        , padding (px 5)
        , borderRadius (px 5)
        , boxShadow5 (px 0) (px 0) (px 10) (px 2) (rgb 100 100 100) 
        ]
    , class PanelToggle
        [ property "writing-mode" "vertical-rl"
        , property "user-select" "none"
        , marginBottom (px 2)
        , hover
            [ cursor default
            ]
        ]
    , class ParameterInput
        [ paddingTop (px 5)
        , paddingBottom (px 5)
        , children
            [ each [ input, textarea ]
                [ marginTop (px 2)
                ]
            ,
            textarea
                [ fontFamilies [ "Source Code Pro", "monospace" ]
                , resize none
                ]
            ]
        ]
    , id AppHeader
        [ fontFamilies [ "Source Code Pro", "monospace" ]
        , color colorPalette.c1
        , backgroundColor colorPalette.c4
        , boxShadow5 (px 0) (px 0) (px 10) (px 2) (rgb 100 100 100)
        ]
    , id Build
        [ paddingRight (px 12)
        ]
    , id Examples
        [ textAlign center
        ]
    , id BuildingStatus
        [ textAlign center
        , marginTop (px -40)
        , marginLeft (px -40)
        ]
    , id ParameterHistory
        [ children
            [ Css.Elements.table
                [ fontSize (pt 10)
                ]
            , tbody
                [ fontFamilies [ "Source Code Pro", "monospace" ]
                , hover
                    [ backgroundColor colorPalette.c3
                    , cursor default
                    ]
                ]
            ]
        ]
    ]


type alias ColorPalette =
    { c1 : Css.Color
    , c2 : Css.Color
    , c3 : Css.Color
    , c4 : Css.Color
    , c5 : Css.Color
    }


colorPalette : ColorPalette
colorPalette =
    { c1 = hex "e6e8e6"
    , c2 = hex "ced0ce"
    , c3 = hex "9fb8ad"
    , c4 = hex "475841"
    , c5 = hex "3f403f"
    }
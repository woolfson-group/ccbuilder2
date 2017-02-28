module BuilderCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, h1, h2, h3, table)


type CssClasses
    = OverlayPanel
    | PanelToggle
    | ParameterInput


type CssIds
    = Page


css : Stylesheet
css =
    stylesheet
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
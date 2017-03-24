module BuilderCss exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Css.Elements exposing (
    body, h1, h2, h3, h4, table, tbody, tr, input, textarea)
import Types exposing (Panel(..))


cssNamespace : String
cssNamespace = "builder"


type CssClasses
    = OverlayPanelCss
    | LeftPanelToggleCss
    | RightPanelToggleCss
    | ParameterInputCss
    | FlexContainerCss
    | FlexItemCss
    | CCBButtonCss


css : Stylesheet
css =
    ( stylesheet << namespace cssNamespace )
    [ body
        [ color colorPalette.c5
        , margin (px 0)
        , padding (px 0)
        ]
    , h1
        [ fontFamilies [ "Russo One", "sans-serif" ]
        , margin (px 0)
        , paddingLeft (px 10)
        ]
    , h4
        [ textDecoration underline 
        ]
    , each [ h2, h3, h4 ]
        [ marginTop (px 2)
        , marginBottom (px 0)
        ]
    , Css.Elements.table
        [ borderCollapse collapse
        ]
    , class OverlayPanelCss
        [ fontFamilies [ "Roboto", "sans-serif" ]
        , backgroundColor colorPalette.c2
        , padding (px 5)
        , borderRadius (px 5)
        , boxShadow5 (px 0) (px 0) (px 10) (px 2) (rgb 100 100 100) 
        ]
    , class LeftPanelToggleCss
        [ property "writing-mode" "vertical-rl"
        , property "-webkit-writing-mode" "vertical-rl"
        , property "-ms-writing-mode" "vertical-rl"
        , property "user-select" "none"
        , property "-ms-user-select" "none"
        , property "-webkit-user-select" "none"
        , marginBottom (px 2)
        , borderRadius4 (px 0) (px 5) (px 5) (px 0)
        , hover
            [ cursor default
            ]
        ]
    , class RightPanelToggleCss
        [ property "writing-mode" "vertical-rl"
        , property "-webkit-writing-mode" "vertical-rl"
        , property "-ms-writing-mode" "vertical-rl"
        , property "user-select" "none"
        , property "-ms-user-select" "none"
        , property "-webkit-user-select" "none"
        , float right
        , marginBottom (px 2)
        , borderRadius4 (px 5) (px 0) (px 0) (px 5)
        , hover
            [ cursor default
            ]
        ]
    , class ParameterInputCss
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
    , class FlexContainerCss
        [ displayFlex
        , flexDirection row
        , children
            [ class FlexItemCss
                [ flex none
                ]
            ]
        ]
    , class CCBButtonCss
        [ borderRadius (px 5)
        , fontFamilies [ "Roboto", "sans-serif" ]
        , backgroundColor colorPalette.c2
        , textDecoration none
        , marginRight (px 2)
        , padding (px 2)
        , hover
            [ backgroundColor colorPalette.c3
            , textDecoration none
            ]
        ]
    , id AppHeaderPanel
        [ fontFamilies [ "Source Code Pro", "monospace" ]
        , color colorPalette.c1
        , backgroundColor colorPalette.c4
        , boxShadow5 (px 0) (px 0) (px 10) (px 2) (rgb 100 100 100)
        ]
    , id BuildPanel
        [ paddingRight (px 12)
        ]
    , id ExamplesPanel
        [ textAlign center
        ]
    , id BuildingStatusPanel
        [ textAlign center
        , marginTop (px -40)
        , marginLeft (px -40)
        ]
    , id BuildHistoryPanel
        [ children
            [ Css.Elements.table
                [ fontSize (pt 10)
                ]
            , Css.Elements.table
                [ children
                    [ tbody
                        [ children
                            [ tr
                                [ fontFamilies [ "Source Code Pro", "monospace" ]
                                , hover
                                    [ backgroundColor colorPalette.c3
                                    , cursor default
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]


panelStyling : List Mixin
panelStyling = [ position absolute, zIndex (int 1) ]


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
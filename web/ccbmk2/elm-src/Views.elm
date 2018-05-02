module Views exposing (..)

import BuilderCss exposing (CssClasses(..), cssNamespace, panelStyling)
import BuildPanel
import Css
import Css.Colors
import Dict
import ExamplesPanel
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import Model exposing (..)
import Markdown
import Types
    exposing
        ( Msg(..)
        , ParameterRecord
        , HistoryID
        , HistoryEntry
        , ParametersDict
        , Parameter(..)
        , HelixType(..)
        , BuildMode(..)
        , OptStatus(..)
        , Panel(..)
        , Representation
        , RepOption(..)
        , InfoBoxID(..)
        , optStatusToString
        , emptyParameterRecord
        )


{ class, classList, id } =
    Html.CssHelpers.withNamespace cssNamespace


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


view : Model -> Html Msg
view model =
    div [ id "viewer", styles viewerStyling ] [ overlayPanels model ]


viewerStyling : List Css.Mixin
viewerStyling =
    [ Css.position Css.fixed
    , Css.bottom (Css.px 0)
    , Css.top (Css.px 0)
    , Css.left (Css.px 0)
    , Css.right (Css.px 0)
    ]


overlayPanels : Model -> Html Msg
overlayPanels model =
    let
        panelDivs =
            [ siteHeader
            , topLeftToggles
            , topRightToggles
            , bottomRightToggles
            , BuildPanel.buildPanel
                model.helixType
                model.buildMode
                model.parameters
                model.currentInput
                model.building
                model.panelVisibility.buildPanel
            , optimisePanel model
            , ExamplesPanel.examplesPanel
                model.building
                model.panelVisibility.examplesPanel
            , buildingStatusPanel model.building
            , buildHistoryPanel
                model.modelHistory
                model.building
                model.panelVisibility.buildHistoryPanel
            , viewerPanel model.panelVisibility.viewerPanel
            , modelInfoGroup model
            , aboutPanel model.panelVisibility.aboutPanel
            ]
                ++ (List.length model.optJobs
                        |> List.range 1
                        |> List.map2 optJobStatus model.optJobs
                   )
    in
        div [] panelDivs


siteHeader : Html msg
siteHeader =
    div
        [ class [ FlexContainerCss ]
        , id [ AppHeaderPanel ]
        , styles <| headerStyling ++ panelStyling
        ]
        [ header [ styles [ Css.width (Css.pct 50) ] ] [ h1 [] [ text "CCBuilder 2.0" ] ]
        , div
            [ styles
                [ Css.width (Css.pct 50)
                , Css.textAlign Css.right
                , Css.paddingRight (Css.px 10)
                ]
            ]
            [ h3
                [ styles
                    [ Css.position Css.absolute
                    , Css.right (Css.px 50)
                    ]
                ]
                [ text "Powered by" ]
            , a
                [ href "https://github.com/woolfson-group/isambard" ]
                [ img
                    [ styles
                        [ Css.height (Css.pct 80)
                        , Css.position Css.absolute
                        , Css.top (Css.pct 10)
                        , Css.right (Css.px 3)
                        , Css.borderRadius (Css.px 3)
                        ]
                    , src "static/images/logo_small.png"
                    ]
                    []
                ]
            ]
        ]


headerStyling : List Css.Mixin
headerStyling =
    [ Css.position Css.absolute
    , Css.lineHeight (Css.px 50)
    , Css.top (Css.px 0)
    , Css.left (Css.px 0)
    , Css.width (Css.pct 100)
    ]


topLeftToggles : Html Msg
topLeftToggles =
    div [ styles topLeftTogglesStyling ]
        [ BuildPanel.toggleBuildPanel
        , ExamplesPanel.toggleExamplesPanel
        , toggleOptimisePanel
        ]


topLeftTogglesStyling : List Css.Mixin
topLeftTogglesStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 0)
    , Css.zIndex (Css.int 2)
    , Css.position Css.absolute
    ]


topRightToggles : Html Msg
topRightToggles =
    div [ styles topRightTogglesStyling ]
        [ toggleBuildHistoryPanel
        , toggleViewerPanel
        ]


topRightTogglesStyling : List Css.Mixin
topRightTogglesStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px 0)
    , Css.zIndex (Css.int 2)
    , Css.position Css.absolute
    , Css.width (Css.px 30)
    ]


bottomRightToggles : Html Msg
bottomRightToggles =
    div [ styles bottomRightTogglesStyling ]
        [ toggleAboutPanel
        ]


bottomRightTogglesStyling : List Css.Mixin
bottomRightTogglesStyling =
    [ Css.bottom (Css.px 20)
    , Css.right (Css.px 0)
    , Css.zIndex (Css.int 2)
    , Css.position Css.absolute
    , Css.width (Css.px 30)
    ]



-- Optimise Panel


optimisePanel : Model -> Html Msg
optimisePanel model =
    let
        visible =
            model.panelVisibility.optimisePanel

        advancedBuild =
            case model.buildMode of
                Basic ->
                    False

                Advanced ->
                    True

        optimising =
            if List.length model.optJobs > 0 then
                True
            else
                False

        totalResidueCount =
            Dict.values model.parameters
                |> List.map .sequence
                |> List.map (Maybe.withDefault "")
                |> List.map String.length
                |> List.sum

        aboveResLimit =
            if totalResidueCount > 240 then
                True
            else
                False

        disabledOpt =
            advancedBuild || optimising || aboveResLimit
    in
        div
            [ class [ OverlayPanelCss ]
            , styles <| panelStyling ++ optimisePanelStyling
            , hidden <| not visible
            ]
            ([ h3 [] [ text "Optimise Parameters" ]
             , hr [] []

             {- Removing until fix
                , text "Heat"
                , br [] []
                , input
                   [ type_ "range"
                   , Html.Attributes.min "0"
                   , Html.Attributes.max "2000"
                   , value (toString model.heat)
                   , onInput SetHeat
                   ]
                   []
                , br [] []
             -}
             ]
                ++ [ button
                        [ onClick Optimise, disabled disabledOpt ]
                        [ text "Optimise Model" ]
                   ]
                ++ (if advancedBuild then
                        [ hr [] []
                        , h3 [] [ text "Warning" ]
                        , Markdown.toHtml
                            [ styles [ Css.color (Css.Colors.red) ]
                            ]
                            ("Optimisation cannot be performed in advanced mode"
                                ++ ". If you'd like to run larger more complex"
                                ++ " optimisations, please "
                                ++ "consider using [ISAMBARD](https://github."
                                ++ "com/woolfson-group/isambard)."
                            )
                        ]
                    else if aboveResLimit then
                        [ hr [] []
                        , h3 [] [ text "Warning" ]
                        , Markdown.toHtml
                            [ styles [ Css.color (Css.Colors.red) ]
                            ]
                            ("Due to available compute, only models with 240 "
                                ++ "or less residues can be optimised. If you'd"
                                ++ " like to run larger optimisations, please "
                                ++ "consider using [ISAMBARD](https://github."
                                ++ "com/woolfson-group/isambard)."
                            )
                        ]
                    else
                        []
                   )
            )


optimisePanelStyling : List Css.Mixin
optimisePanelStyling =
    [ Css.top (Css.px 60)
    , Css.left (Css.px 35)
    , Css.maxWidth (Css.pct 30)
    ]


toggleOptimisePanel : Html Msg
toggleOptimisePanel =
    div
        [ class [ OverlayPanelCss, LeftPanelToggleCss ]
        , onClick (TogglePanel OptimisePanel)
        ]
        [ text "Optimise" ]



-- Model Info


modelInfoGroup : Model -> Html Msg
modelInfoGroup model =
    div
        [ class [ FlexContainerCss ]
        , styles <| modelInfoGroupPanelStyling ++ panelStyling
        ]
        ([ modelInfoPanel model
         ]
            ++ (if List.member MIBudeEnergy model.activeInfoBoxes then
                    [ budeEnergyInfo ]
                else if List.member MIRPT model.activeInfoBoxes then
                    [ rptInfo ]
                else if List.member MIHLKnobs model.activeInfoBoxes then
                    [ highlightKIHInfo ]
                else
                    []
               )
        )


modelInfoPanel : Model -> Html Msg
modelInfoPanel model =
    div
        [ class [ OverlayPanelCss, FlexItemCss ]
        , styles [ Css.bottom (Css.px 0) ]
        ]
        [ h3 [] [ text "Model Information" ]
        , hr [] []
        , infoText MIBudeEnergy "BUDE Energy �"
        , Maybe.map (roundToXDecPlaces 1) model.score
            |> Maybe.map toString
            |> Maybe.withDefault ""
            |> \val -> input [ value val, readonly True ] []
        , hr [] []
        , infoText MIRPT "Residues per Turn �"
        , Maybe.map (roundToXDecPlaces 2) model.residuesPerTurn
            |> Maybe.map toString
            |> Maybe.withDefault ""
            |> \val -> input [ value val, readonly True ] []
        , hr [] []
        , button
            [ class [ CCBButtonCss ]
            , styles [ Css.float Css.left ]
            , onClick HighlightKnobs
            ]
            [ text "Highlight Knobs" ]
        , infoText MIHLKnobs "�"
        , hr [] []
        , downloadStructureButton model.pdbFile
        ]


budeEnergyInfo : Html msg
budeEnergyInfo =
    div
        [ class [ OverlayPanelCss, FlexItemCss ]
        , styles infoBoxStyling
        ]
        [ h3 [] [ text "BUDE Energy" ]
        , text mIBudeEnergyText
        ]


mIBudeEnergyText : String
mIBudeEnergyText =
    """The interaction energy between helices as calculated by the BUDE force field implemented in the BUFF module of ISAMBARD."""


rptInfo : Html msg
rptInfo =
    div
        [ class [ OverlayPanelCss, FlexItemCss ]
        , styles infoBoxStyling
        ]
        [ h3 [] [ text "Residues Per Turn" ]
        , text mIRPTText
        ]


mIRPTText : String
mIRPTText =
    """Residues per turn of the alpha helix, can be used as a proxy for backbone strain."""


highlightKIHInfo : Html msg
highlightKIHInfo =
    div
        [ class [ OverlayPanelCss, FlexItemCss ]
        , styles infoBoxStyling
        ]
        [ h3 [] [ text "Highlight Knobs" ]
        , text highlightKIHText
        ]


highlightKIHText : String
highlightKIHText =
    """Highlights knob residues red, all other residues are coloured blue."""


modelInfoGroupPanelStyling : List Css.Mixin
modelInfoGroupPanelStyling =
    [ Css.bottom (Css.px 0)
    , Css.left (Css.px 0)
    ]


infoBoxStyling : List Css.Mixin
infoBoxStyling =
    [ Css.maxWidth (Css.pct 40)
    , Css.maxHeight (Css.em 8)
    , Css.lineHeight (Css.em 1)
    ]


roundToXDecPlaces : Int -> Float -> Float
roundToXDecPlaces precision num =
    let
        scaling =
            10 ^ precision |> toFloat
    in
        round (num * scaling)
            |> toFloat
            |> flip (/) scaling


downloadStructureButton : Maybe String -> Html Msg
downloadStructureButton pdbFile =
    let
        deactivated =
            if pdbFile == Nothing then
                True
            else
                False
    in
        button
            [ class [ CCBButtonCss ]
            , onClick DownloadPdb
            , disabled deactivated
            ]
            [ text "Download PDB" ]



-- Build History


buildHistoryPanel : Dict.Dict Int HistoryEntry -> Bool -> Bool -> Html Msg
buildHistoryPanel modelHistory building visible =
    div
        [ class [ OverlayPanelCss ]
        , id [ BuildHistoryPanel ]
        , styles <| panelStyling ++ buildHistoryPanelStyling
        , hidden <| not visible
        ]
        [ h2 [] [ text "Build History" ]
        , hr [] []
        , table []
            [ modelDetailTableHeader
            , List.map2 modelParametersAsRow
                (Dict.toList modelHistory |> List.reverse)
                (List.repeat (List.length <| Dict.toList modelHistory) building)
                |> List.concat
                |> tbody []
            ]
        ]


modelDetailTableHeader : Html msg
modelDetailTableHeader =
    thead []
        [ tr []
            [ th [] []
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Radius" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Pitch" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Interface Angle" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Super-Helical Rotation" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Z-Shift" ]
            , th [] [ text "Sequence" ]
            , th [ styles [ Css.width (Css.em 6) ] ] [ text "Register" ]
            , th [] [ text "BUDE Score" ]
            , th [] []
            ]
        ]


modelParametersAsRow : ( HistoryID, HistoryEntry ) -> Bool -> List (Html Msg)
modelParametersAsRow ( hID, ( parameters, visible, score, helixType, buildMode, _ ) ) building =
    let
        foldedRows =
            Dict.values parameters
                |> List.tail
                |> Maybe.withDefault []
                |> List.map (modelFoldedRow score)
    in
        if not visible then
            [ modelHistoryTopRow hID parameters building visible score helixType buildMode ]
        else
            (modelHistoryTopRow hID parameters building visible score helixType buildMode)
                :: foldedRows


modelHistoryTopRow :
    HistoryID
    -> ParametersDict
    -> Bool
    -> Bool
    -> Float
    -> HelixType
    -> BuildMode
    -> Html Msg
modelHistoryTopRow hID parameters building visible score helixType buildMode =
    let
        topRowParameters =
            Dict.values parameters
                |> List.head
                |> Maybe.withDefault emptyParameterRecord
    in
        tr
            []
            ([ div
                [ onClick (ExpandHistory hID) ]
                [ if (not visible) then
                    text "▶"
                  else
                    text "▼"
                ]
             ]
                ++ List.map makeParameterTh
                    (parametersToRow topRowParameters score)
                ++ [ button
                        [ class [ CCBButtonCss ]
                        , onClick (SetParametersAndBuild parameters helixType buildMode)
                        , disabled building
                        ]
                        [ text "Rebuild" ]
                   ]
            )


modelFoldedRow : Float -> ParameterRecord -> Html Msg
modelFoldedRow score parameters =
    tr
        []
        ([ text " ┋"
         ]
            ++ List.map makeParameterTh
                (parametersToRow parameters score)
        )


parametersToRow : ParameterRecord -> Float -> List String
parametersToRow parameters score =
    [ parameters.radius
        |> Maybe.withDefault 0
        |> roundToXDecPlaces 1
        |> toString
    , parameters.pitch
        |> Maybe.withDefault 0
        |> round
        |> toString
    , parameters.phiCA
        |> Maybe.withDefault 0
        |> roundToXDecPlaces 1
        |> toString
    , parameters.superHelRot
        |> Maybe.withDefault 0
        |> roundToXDecPlaces 1
        |> toString
    , parameters.zShift
        |> Maybe.withDefault 0
        |> roundToXDecPlaces 1
        |> toString
    , parameters.sequence |> Maybe.withDefault ""
    , parameters.register
    , toString <| roundToXDecPlaces 2 score
    ]


makeParameterTh : String -> Html Msg
makeParameterTh pString =
    text pString
        |> List.singleton
        |> th []


buildHistoryPanelStyling : List Css.Mixin
buildHistoryPanelStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px 35)
    ]


toggleBuildHistoryPanel : Html Msg
toggleBuildHistoryPanel =
    div
        [ class [ OverlayPanelCss, RightPanelToggleCss ]
        , onClick (TogglePanel BuildHistoryPanel)
        ]
        [ text "Build History" ]



-- Viewer Panel


viewerPanel : Bool -> Html Msg
viewerPanel visible =
    div
        [ class [ OverlayPanelCss ]
        , id [ ViewerPanel ]
        , styles <| panelStyling ++ viewerPanelStyling
        , hidden <| not visible
        ]
        [ h2 [] [ text "Viewer Options" ]
        , hr [] []
        , h3 [] [ text "Representation" ]
        , hr [] []
        , text "Backbone"
        , br [] []
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation Cartoon)
            ]
            [ text "Cartoon" ]
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation Trace)
            ]
            [ text "Trace" ]
        , br [] []
        , text "All Atoms"
        , br [] []
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation BallsAndSticks)
            ]
            [ text "Balls and Sticks" ]
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation Spheres)
            ]
            [ text "Spheres" ]
        , button
            [ class [ CCBButtonCss ]
            , onClick (EditRepresentation Points)
            ]
            [ text "Dots" ]
        , hr [] []
        , h3 [] [ text "Other Options" ]
        , hr [] []
        , button
            [ class [ CCBButtonCss ]
            , onClick ShowAxes
            ]
            [ text "Axes" ]
        , text " (xyz = rgb)"
        ]


viewerPanelStyling : List Css.Mixin
viewerPanelStyling =
    [ Css.top (Css.px 60)
    , Css.right (Css.px 35)
    ]


toggleViewerPanel : Html Msg
toggleViewerPanel =
    div
        [ class [ OverlayPanelCss, RightPanelToggleCss ]
        , onClick (TogglePanel ViewerPanel)
        ]
        [ text "Viewer" ]



-- Info Box


infoText : InfoBoxID -> String -> Html Msg
infoText infoBoxID contentText =
    div
        [ onMouseEnter (ShowInfo infoBoxID)
        , onMouseLeave (CloseInfo infoBoxID)
        ]
        [ text contentText ]



-- Building Status


buildingStatusPanel : Bool -> Html msg
buildingStatusPanel building =
    div
        [ class [ OverlayPanelCss ]
        , id [ BuildingStatusPanel ]
        , styles <| buildingStatusStyling ++ panelStyling
        , hidden (not building)
        ]
        [ text "Building"
        , img [ src "static/css/infinity.gif", width 80, height 80 ] []
        ]


buildingStatusStyling : List Css.Mixin
buildingStatusStyling =
    [ Css.top (Css.pct 50)
    , Css.left (Css.pct 50)
    , Css.width (Css.px 80)
    , Css.height (Css.px 80)
    ]



-- Optimisation Job


optJobStatus : ( String, OptStatus ) -> Int -> Html Msg
optJobStatus ( optID, status ) position =
    div
        [ class [ OverlayPanelCss ]
        , styles <| optJobStatusStyling position ++ panelStyling
        ]
        ([ text "Optimising"
         , br [] []
         , text ("(" ++ (toString status) ++ ")")
         ]
            ++ (case status of
                    Complete ->
                        [ button
                            [ onClick (RetrieveOptimisation optID) ]
                            [ text "Retrieve" ]
                        ]

                    Failed ->
                        [ button
                            [ onClick (ClearOptimisation optID) ]
                            [ text "Clear" ]
                        ]

                    _ ->
                        [ img
                            [ src "static/css/infinity.gif", width 80, height 80 ]
                            []
                        ]
               )
        )


optJobStatusStyling : Int -> List Css.Mixin
optJobStatusStyling position =
    [ Css.textAlign Css.center
    , Css.bottom (Css.px 20)
    , Css.right (Css.px 35)
    , Css.width (Css.px 90)
    ]



-- About Panel


aboutPanel : Bool -> Html Msg
aboutPanel visible =
    div
        [ class [ OverlayPanelCss ]
        , styles <| panelStyling ++ aboutPanelStyling
        , hidden <| not visible
        ]
        [ h2 [] [ text "About (Ver 2.0.4)" ]
        , hr [] []
        , Markdown.toHtml [] aboutText
        ]


aboutText : String
aboutText =
    """CCBuilder is developed and maintained by the [Woolfson Group, University
of Bristol](http://www.chm.bris.ac.uk/org/woolfson/). The project is open
source with code available on
[GitHub](https://github.com/woolfson-group/ccbuilder2). If you
encounter any bugs or have ideas for enhancements to CCBuilder 2.0, please
either create an issue on GitHub or contact chris.wood@bris.ac.uk.

### Citation

[Wood CW and Woolfson DN (2017) CCBuilder 2.0: Powerful and accessible
coiled-coil modelling,
_Protein Science_. doi: 10.1002/pro.3279](http://dx.doi.org/10.1002/pro.3279)

### Useful References

[Wood CW _et al_ (2017) ISAMBARD: an open-source computational environment
for biomolecular analysis, modelling and design, Bioinformatics.
doi: 10.1093/bioinformatics/btx352](
https://doi.org/10.1093/bioinformatics/btx352)
"""


aboutPanelStyling : List Css.Mixin
aboutPanelStyling =
    [ Css.bottom (Css.px 20)
    , Css.right (Css.px 35)
    , Css.maxWidth (Css.pct 40)
    ]


toggleAboutPanel : Html Msg
toggleAboutPanel =
    div
        [ class [ OverlayPanelCss, RightPanelToggleCss ]
        , onClick (TogglePanel AboutPanel)
        ]
        [ text "About" ]

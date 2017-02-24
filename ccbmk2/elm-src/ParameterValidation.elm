module ParameterValidation exposing (allParametersValid, editParameterValue, currentParameterValues)

import String
import Types
    exposing
        ( ParameterRecord
        , Parameter(..)
        )


currentParameterValues : ParameterRecord ->  (String, String, String, String, String)
currentParameterValues { oligomerState, radius, pitch, phiCA, sequence } =
    let
        seq = Maybe.withDefault "" sequence
    in
        ( maybeValueToString oligomerState
        , maybeValueToString radius
        , maybeValueToString pitch
        , maybeValueToString phiCA
        , seq
        )


maybeValueToString : Maybe number -> String
maybeValueToString mVal =
    case mVal of
        Just val -> toString val
        Nothing -> ""


allParametersValid : ParameterRecord -> Bool
allParametersValid { oligomerState, radius, pitch, phiCA, sequence } =
    let
        vOS =
            oligomerState /= Nothing

        vRadius =
            radius /= Nothing

        vPitch =
            pitch /= Nothing

        vPhiCA =
            phiCA /= Nothing

        vSeq =
            sequence /= Nothing
    in
    List.member False [ vOS, vRadius, vPitch, vPhiCA, vSeq ]


editParameterValue : ParameterRecord -> Parameter -> String -> ParameterRecord
editParameterValue parameters parameter newValue =
    case parameter of
        OligomerState ->
            let
                postValOS =
                    String.toInt newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validateOligomerState
            in
                { parameters | oligomerState = postValOS }

        Radius ->
            let
                postValRadius =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validateRadius
            in
                { parameters | radius = postValRadius }

        Pitch ->
            let
                postValPitch =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validatePitch
            in
                { parameters | pitch = postValPitch }

        PhiCA ->
            let
                postValPhiCA =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validatePhiCA
            in
                { parameters | phiCA = postValPhiCA }

        Sequence ->
            { parameters | sequence = validateSequence newValue }


validateOligomerState : Int -> Maybe Int
validateOligomerState os =
    if (os > 0) && (isNotNaN <| toFloat os) then
        Just os
    else
        Nothing


validateRadius : Float -> Maybe Float
validateRadius radius =
    if (radius > 0) && (isNotNaN radius) then
        Just radius
    else
        Nothing


validatePitch : Float -> Maybe Float
validatePitch pitch =
    if (pitch > 0) && (isNotNaN pitch) then
        Just pitch
    else
        Nothing


validatePhiCA : Float -> Maybe Float
validatePhiCA phica =
    if isNotNaN phica then
        Just phica
    else
        Nothing


validateSequence : String -> Maybe String
validateSequence sequence =
    let
        allValidChars =
            String.toUpper sequence
                |> String.toList
                |> List.all isAllowedSeqChar
    in
        if allValidChars && (String.length sequence > 0) then
            Just (String.toUpper sequence)
        else
            Nothing


isAllowedSeqChar : Char -> Bool
isAllowedSeqChar char =
    let
        allowed =
            [ 'A'
            , 'C'
            , 'D'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'I'
            , 'K'
            , 'L'
            , 'M'
            , 'N'
            , 'P'
            , 'Q'
            , 'R'
            , 'S'
            , 'T'
            , 'V'
            , 'W'
            , 'Y'
            ]
    in
        List.member char allowed


isNotNaN : Float -> Bool
isNotNaN =
    not << isNaN

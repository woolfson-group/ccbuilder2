module ParameterValidation exposing (containsInvalidParameter, editParameterValue)

import String
import Types
    exposing
        ( ParameterRecord
        , InputValues
        , Parameter(..)
        )


containsInvalidParameter : ParameterRecord -> Bool
containsInvalidParameter { oligomerState, radius, pitch, phiCA, sequence } =
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


editParameterValue : ParameterRecord -> InputValues -> Parameter -> String -> ( ParameterRecord, InputValues )
editParameterValue parameters currentInput parameter newValue =
    case parameter of
        OligomerState ->
            let
                postValOS =
                    String.toInt newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validateOligomerState

                newParameters =
                    { parameters | oligomerState = postValOS }

                newInput =
                    { currentInput | oligomerState = newValue }
            in
                ( newParameters, newInput )

        Radius ->
            let
                postValRadius =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validateRadius

                newParameters =
                    { parameters | radius = postValRadius }

                newInput =
                    { currentInput | radius = newValue }
            in
                ( newParameters, newInput )

        Pitch ->
            let
                postValPitch =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validatePitch

                newParameters =
                    { parameters | pitch = postValPitch }

                newInput =
                    { currentInput | pitch = newValue }
            in
                ( newParameters, newInput )

        PhiCA ->
            let
                postValPhiCA =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validatePhiCA

                newParameters =
                    { parameters | phiCA = postValPhiCA }

                newInput =
                    { currentInput | phiCA = newValue }
            in
                ( newParameters, newInput )

        Sequence ->
            let
                newParameters =
                    { parameters | sequence = validateSequence newValue }

                newInput =
                    { currentInput | sequence = newValue }
            in
                ( newParameters, newInput )


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

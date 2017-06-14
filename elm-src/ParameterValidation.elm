module ParameterValidation
    exposing
        ( containsInvalidParameter
        , editParameterValue
        , invalidParameterDict
        )

import Dict
import String
import Types
    exposing
        ( ParameterRecord
        , ParametersDict
        , InputValues
        , Parameter(..)
        )


invalidParameterDict : ParametersDict -> Bool
invalidParameterDict parameters =
    Dict.values parameters
        |> List.map containsInvalidParameter
        |> List.any (\b -> b == True)


containsInvalidParameter : ParameterRecord -> Bool
containsInvalidParameter { radius, pitch, phiCA, sequence, superHelRot, zShift } =
    let
        vRadius =
            radius /= Nothing

        vPitch =
            pitch /= Nothing

        vPhiCA =
            phiCA /= Nothing

        vSeq =
            sequence /= Nothing

        vSuperHelRot =
            superHelRot /= Nothing

        vZShift =
            zShift /= Nothing
    in
        List.member False [ vRadius, vPitch, vPhiCA, vSeq, vSuperHelRot, vZShift ]


editParameterValue : ParameterRecord -> InputValues -> Parameter -> String -> ( ParameterRecord, InputValues )
editParameterValue parameters currentInput parameter newValue =
    case parameter of
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
                validatedSequence =
                    validateSequence newValue

                newParameters =
                    { parameters | sequence = validatedSequence }

                newInput =
                    { currentInput | sequence = newValue }
            in
                ( newParameters, newInput )

        Register ->
            let
                newParameters =
                    { parameters | register = newValue }

                newInput =
                    { currentInput | register = newValue }
            in
                ( newParameters, newInput )

        SuperHelicalRotation ->
            let
                postValSHR =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validateSuperHelicalRot

                newParameters =
                    { parameters | superHelRot = postValSHR }

                newInput =
                    { currentInput | superHelRot = newValue }
            in
                ( newParameters, newInput )

        Orientation ->
            let
                newParameters =
                    { parameters | antiParallel = not parameters.antiParallel }

                newInput =
                    { currentInput | antiParallel = toString newParameters.antiParallel }
            in
                ( newParameters, newInput )

        ZShift ->
            let
                postValZShift =
                    String.toFloat newValue
                        |> Result.toMaybe
                        |> Maybe.andThen validateZShift

                newParameters =
                    { parameters | zShift = postValZShift }

                newInput =
                    { currentInput | zShift = newValue }
            in
                ( newParameters, newInput )

        LinkedSuperHelRot ->
            let
                newParameters =
                    { parameters
                        | linkedSuperHelRot = not parameters.antiParallel
                    }

                newInput =
                    { currentInput
                        | linkedSuperHelRot = toString newParameters.linkedSuperHelRot
                    }
            in
                ( newParameters, newInput )


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
        processedSequence =
            sequence
                |> String.toUpper
                |> String.trim
                |> String.words
                |> String.join ""

        allValidChars =
            processedSequence
                |> String.toList
                |> List.all isAllowedSeqChar
    in
        if allValidChars && (String.length processedSequence > 0) then
            Just (processedSequence)
        else
            Nothing


validateSuperHelicalRot : Float -> Maybe Float
validateSuperHelicalRot superHelRot =
    if isNotNaN superHelRot then
        Just superHelRot
    else
        Nothing


validateZShift : Float -> Maybe Float
validateZShift zShift =
    if isNotNaN zShift then
        Just zShift
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

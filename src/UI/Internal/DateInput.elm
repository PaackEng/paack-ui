module UI.Internal.DateInput exposing
    ( DateInput(..)
    , PeriodComparison(..)
    , PeriodDate
    , RangeDate
    , fromPosix
    , isPosixAfter
    , isPosixBefore
    , isPosixBetween
    , isPosixEqual
    , parseDD_MM_YYYY
    , toDD_MM_YYYY
    , toTextField
    )

import Time
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.RenderConfig exposing (RenderConfig)
import UI.TextField as TextField exposing (TextField)


type PeriodComparison
    = On
    | Before
    | After


type alias PeriodDate =
    { date : DateInput, comparison : PeriodComparison }


type DateInput
    = DateValid PrimitiveDate
    | DateInvalid String


type alias PrimitiveDate =
    { year : Int, month : Time.Month, day : Int }


type alias RangeDate =
    { from : DateInput, to : DateInput }


{-|

               _      ______ _____ _______
         /\   | |    |  ____|  __ \__   __|
        /  \  | |    | |__  | |__) | | |
       / /\ \ | |    |  __| |  _  /  | |
      / ____ \| |____| |____| | \ \  | |
     /_/    \_\______|______|_|  \_\ |_|


    This funciton doesn't validate impossible dates like 31/02/2020

-}
parseDD_MM_YYYY : String -> String -> DateInput
parseDD_MM_YYYY separator inputString =
    case String.split separator inputString of
        [ dayStr, monthStr, yearStr ] ->
            case ( String.toInt dayStr, parseMonthNumber monthStr, String.toInt yearStr ) of
                ( Just day, Just month, Just year ) ->
                    if day < 1 || day > 31 then
                        DateInvalid inputString

                    else if year < 1 then
                        DateInvalid inputString

                    else if year > 1970 then
                        DateValid { year = year, month = month, day = day }

                    else
                        DateInvalid inputString

                _ ->
                    DateInvalid inputString

        _ ->
            DateInvalid inputString


toDD_MM_YYYY : String -> DateInput -> String
toDD_MM_YYYY separator date =
    case date of
        DateInvalid string ->
            string

        DateValid { year, month, day } ->
            (day |> String.fromInt |> String.padLeft 2 '0')
                ++ separator
                ++ (month |> monthToInt |> String.fromInt |> String.padLeft 2 '0')
                ++ separator
                ++ (year |> String.fromInt)


fromPosix : Time.Zone -> Time.Posix -> DateInput
fromPosix timeZone posix =
    DateValid
        { year = Time.toYear timeZone posix
        , month = Time.toMonth timeZone posix
        , day = Time.toDay timeZone posix
        }


isPosixEqual : DateInput -> Time.Zone -> Time.Posix -> Bool
isPosixEqual date timeZone posix =
    case date of
        DateValid { year, month, day } ->
            (year == Time.toYear timeZone posix)
                && (month == Time.toMonth timeZone posix)
                && (day == Time.toDay timeZone posix)

        DateInvalid _ ->
            False


isPosixBetween : Time.Zone -> Time.Posix -> DateInput -> DateInput -> Bool
isPosixBetween timeZone posix date1 date2 =
    case ( date1, date2 ) of
        ( DateValid s1, DateValid s2 ) ->
            let
                value =
                    posixToPrimitiveDate timeZone posix
            in
            dateAfter s1 value && dateBefore s2 value

        _ ->
            False


isPosixBefore : DateInput -> Time.Zone -> Time.Posix -> Bool
isPosixBefore maybeDate timeZone posix =
    case maybeDate of
        DateValid date ->
            posix
                |> posixToPrimitiveDate timeZone
                |> dateBefore date

        DateInvalid _ ->
            False


isPosixAfter : DateInput -> Time.Zone -> Time.Posix -> Bool
isPosixAfter maybeDate timeZone posix =
    case maybeDate of
        DateValid date ->
            posix
                |> posixToPrimitiveDate timeZone
                |> dateAfter date

        DateInvalid _ ->
            False


ifValid : a -> a -> DateInput -> a
ifValid if_ then_ value =
    case value of
        DateValid _ ->
            if_

        DateInvalid _ ->
            then_


toTextField : RenderConfig -> String -> (String -> msg) -> String -> DateInput -> TextField msg
toTextField cfg separator editMsg label current =
    let
        correct =
            current
                |> toDD_MM_YYYY separator
                |> TextField.singlelineText editMsg label

        invalidTerm =
            cfg |> localeTerms >> .dateInput >> .invalid

        invalid =
            TextField.withError invalidTerm correct
    in
    ifValid correct invalid current



-- Internals


posixToPrimitiveDate : Time.Zone -> Time.Posix -> PrimitiveDate
posixToPrimitiveDate timeZone posix =
    { year = Time.toYear timeZone posix
    , month = Time.toMonth timeZone posix
    , day = Time.toDay timeZone posix
    }


parseMonthNumber : String -> Maybe Time.Month
parseMonthNumber inputString =
    case String.toInt inputString of
        Just 1 ->
            Just Time.Jan

        Just 2 ->
            Just Time.Feb

        Just 3 ->
            Just Time.Mar

        Just 4 ->
            Just Time.Apr

        Just 5 ->
            Just Time.May

        Just 6 ->
            Just Time.Jun

        Just 7 ->
            Just Time.Jul

        Just 8 ->
            Just Time.Aug

        Just 9 ->
            Just Time.Sep

        Just 10 ->
            Just Time.Oct

        Just 11 ->
            Just Time.Nov

        Just 12 ->
            Just Time.Dec

        _ ->
            Nothing


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


dateAfter : PrimitiveDate -> PrimitiveDate -> Bool
dateAfter min value =
    if value.year > min.year then
        True

    else if value.year == min.year then
        let
            valueMonth =
                monthToInt value.month

            minMonth =
                monthToInt min.month
        in
        if valueMonth > minMonth then
            True

        else if valueMonth == minMonth then
            value.day >= min.day

        else
            False

    else
        False


dateBefore : PrimitiveDate -> PrimitiveDate -> Bool
dateBefore max value =
    if value.year < max.year then
        True

    else if value.year == max.year then
        let
            valueMonth =
                monthToInt value.month

            maxMonth =
                monthToInt max.month
        in
        if valueMonth < maxMonth then
            True

        else if valueMonth == maxMonth then
            value.day <= max.day

        else
            False

    else
        False

module UI.Internal.Human exposing
    ( Date(..)
    , PeriodComparison(..)
    , PeriodDate
    , RangeDate
    , dateIfValid
    , dateToNumericString
    , isPosixAfterDate
    , isPosixBeforeDate
    , isPosixBetweenDates
    , isPosixEqualDate
    , parseDate
    , posixToValidDate
    )

import Time


type PeriodComparison
    = On
    | Before
    | After


type alias PeriodDate =
    { date : Date, comparison : PeriodComparison }


type Date
    = DateValid PrimitiveDate
    | DateInvalid String


type alias PrimitiveDate =
    { year : Int, month : Time.Month, day : Int }


type alias RangeDate =
    { from : Date, to : Date }


{-|

               _      ______ _____ _______
         /\   | |    |  ____|  __ \__   __|
        /  \  | |    | |__  | |__) | | |
       / /\ \ | |    |  __| |  _  /  | |
      / ____ \| |____| |____| | \ \  | |
     /_/    \_\______|______|_|  \_\ |_|


    This funciton doesn't validate impossible dates like 31/02/2020

-}
parseDate : String -> Date
parseDate inputString =
    case String.split "/" inputString of
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


dateToNumericString : Date -> String
dateToNumericString date =
    case date of
        DateInvalid string ->
            string

        DateValid { year, month, day } ->
            (day |> String.fromInt |> String.padLeft 2 '0')
                ++ "/"
                ++ (month |> monthToInt |> String.fromInt |> String.padLeft 2 '0')
                ++ "/"
                ++ (year |> String.fromInt)


posixToValidDate : Time.Zone -> Time.Posix -> Date
posixToValidDate timeZone posix =
    DateValid
        { year = Time.toYear timeZone posix
        , month = Time.toMonth timeZone posix
        , day = Time.toDay timeZone posix
        }


isPosixEqualDate : Date -> Time.Zone -> Time.Posix -> Bool
isPosixEqualDate date timeZone posix =
    case date of
        DateValid { year, month, day } ->
            (year == Time.toYear timeZone posix)
                && (month == Time.toMonth timeZone posix)
                && (day == Time.toDay timeZone posix)

        DateInvalid _ ->
            False


isPosixBetweenDates : Time.Zone -> Time.Posix -> Date -> Date -> Bool
isPosixBetweenDates timeZone posix date1 date2 =
    case ( date1, date2 ) of
        ( DateValid s1, DateValid s2 ) ->
            let
                value =
                    posixToPrimitiveDate timeZone posix
            in
            dateAfter s1 value && dateBefore s2 value

        _ ->
            False


isPosixBeforeDate : Date -> Time.Zone -> Time.Posix -> Bool
isPosixBeforeDate maybeDate timeZone posix =
    case maybeDate of
        DateValid date ->
            posix
                |> posixToPrimitiveDate timeZone
                |> dateBefore date

        DateInvalid _ ->
            False


isPosixAfterDate : Date -> Time.Zone -> Time.Posix -> Bool
isPosixAfterDate maybeDate timeZone posix =
    case maybeDate of
        DateValid date ->
            posix
                |> posixToPrimitiveDate timeZone
                |> dateAfter date

        DateInvalid _ ->
            False


dateIfValid : a -> a -> Date -> a
dateIfValid if_ then_ value =
    case value of
        DateValid _ ->
            if_

        DateInvalid _ ->
            then_



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
    let
        minMonth =
            monthToInt min.month

        valueMonth =
            monthToInt value.month
    in
    if value.year > min.year then
        True

    else if value.year == min.year then
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
    let
        maxMonth =
            monthToInt max.month

        valueMonth =
            monthToInt value.month
    in
    if value.year < max.year then
        True

    else if value.year == max.year then
        if valueMonth < maxMonth then
            True

        else if valueMonth == maxMonth then
            value.day <= max.day

        else
            False

    else
        False

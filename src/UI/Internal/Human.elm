module UI.Internal.Human exposing (Date(..), dateToNumericString, parseDate, posixToValidDate)

import Time


type Date
    = DateValid { year : Int, month : Time.Month, day : Int }
    | DateInvalid String


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

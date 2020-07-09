module UI.Internal.Human exposing (parseDate)


parseDate : String -> Maybe ( Int, Int, Int )
parseDate inputString =
    case String.split "/" inputString of
        [ dayStr, monthStr, yearStr ] ->
            case ( String.toInt dayStr, String.toInt monthStr, String.toInt yearStr ) of
                ( Just day, Just month, Just unpadYear ) ->
                    if day < 1 || day > 31 then
                        Nothing

                    else if month < 1 || month > 12 then
                        Nothing

                    else if unpadYear < 1 then
                        Nothing

                    else if unpadYear < 100 then
                        Just ( day, month, unpadYear + 2000 )

                    else if unpadYear < 1000 then
                        Nothing

                    else
                        Just ( day, month, unpadYear )

                _ ->
                    Nothing

        _ ->
            Nothing

module UI.DatePicker exposing (DatePicker(..), init, update, datepicker, Msg(..), DateEvent(..))

import Date exposing (Date, Interval(..), Month, Unit(..))
import Element exposing (Element, alignLeft, alignRight, fill, fillPortion, maximum, minimum, padding, paddingXY, px, rgb, spacing, width)
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Json.Decode
import Time exposing (Month(..))
import UI.Button as Button
import UI.Icon as Icon
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text


defaultToday : Date
defaultToday =
    Date.fromCalendarDate 2022 Feb 11


type alias Property msg =
    { today : Date
    , current : Date
    , selected : Maybe Date
    , toExternal : Msg -> msg
    }


type alias DayItem =
    { date : Date
    , isCurrentMonth : Bool
    }


type DatePicker msg
    = DatePicker (Property msg)


init : Time.Posix -> Maybe Date -> (Msg -> msg) -> DatePicker msg
init today selected toExt =
    let
        todayDate = Date.fromPosix Time.utc today
    in
    DatePicker { today = todayDate
    , selected = selected
    , current = case selected of
                    Just selected_ ->
                        selected_
                    Nothing ->
                        todayDate
    , toExternal = toExt }



-- UPDATE


type Msg
    = PreviousMonth
    | NextMonth
    | Selected Date


type DateEvent
    = None
    | Picked Date
    | Current Date


update : Msg -> DatePicker msg -> ( DatePicker msg, DateEvent )
update msg (DatePicker ({ today, current, selected } as model)) =
    case msg of
        PreviousMonth ->
            let
                prev =
                    Date.add Months -1 current
            in
            ( DatePicker { model | current = prev }, Current prev )

        NextMonth ->
            let
                next =
                    Date.add Months 1 current
            in
            ( DatePicker { model | current = next }, Current next )

        Selected date ->
            ( DatePicker { model | today = today, selected = Just date, current = current }, Picked date )

-- VIEW


datepicker : RenderConfig -> DatePicker msg -> Element msg
datepicker renderConfig (DatePicker ({ today, current, selected, toExternal } as model)) =
    let
        numberOfDays =
            daysOfMonth (Date.month model.current) (Date.year model.current)

        fromPreviousMonth =
            dateDaysFromPreviousMonth model.current

        dates =
            fromPreviousMonth
                ++ datesOfTheMonth model.current
                ++ dateDaysFromNextMonth (List.length fromPreviousMonth + numberOfDays) model.current
    in
    Element.column [ width fill]
        [ header renderConfig toExternal model
        , drawDateMatrix dates model
        ]


drawDateMatrix : List DayItem -> Property msg-> Element msg
drawDateMatrix matrix ({ today, selected, current, toExternal } as model) =
    let
        ( first, rest ) =
            split 7 matrix

        ( second, secondRest ) =
            split 7 rest

        ( third, thirdRest ) =
            split 7 secondRest

        ( fourth, fourthRest ) =
            split 7 thirdRest

        ( fifth, sixth ) =
            split 7 fourthRest
    in
    Element.column
        [ spacing 2
        , Element.width
            (fill
                |> minimum 240
            )
        ]
        [ Element.row [ spacing 2, Element.centerX, Element.width fill ] (List.map drawCol weekHeader)
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model toExternal) first
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model toExternal) second
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model toExternal) third
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model toExternal) fourth
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model toExternal) fifth
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model toExternal) sixth
        ]


drawDate : Property msg -> (Msg -> msg) -> DayItem -> Element msg
drawDate { today, selected, current } externalToMsg day =
    let
        isSelected =
            case selected of
                Just date ->
                    date == day.date

                Nothing ->
                    False

    in
    Element.el
        [ Element.width (fillPortion 7)
        , Element.centerX
        , if isSelected then
            Palette.blue700 |> Palette.toBackgroundColor

          else
            Palette.genericWhite |> Palette.toBackgroundColor
        , Element.mouseOver
            [ Palette.gray200 |> Palette.toBackgroundColor
            ]
        , Element.Events.onClick <| externalToMsg <| (Selected day.date)
        , if day.date == today then
            Element.inFront (Element.el [ Element.centerX, Element.alignBottom ] (Element.text "."))

          else
            Element.inFront Element.none
        , Border.rounded 6
        ]
    <|
        Element.column
            [ Element.height (px 32)
            , Element.centerX
            ,
                if day.isCurrentMonth && not isSelected then
                    Palette.blue800 |> Palette.toFontColor
                else if day.isCurrentMonth && isSelected then
                    Palette.genericWhite |> Palette.toFontColor
                else
                    Palette.gray500 |> Palette.toFontColor
            , Font.size 16
            ]
            [ Element.el
                [ Element.centerY
                ]
              <|
                Element.text <|
                    String.fromInt (Date.day day.date)
            ]


drawCol : String -> Element msg
drawCol day =
    Element.el [ Element.width (fillPortion 7), Element.centerX, Element.centerY ] <|
        Element.el [ Element.centerX, Font.size 12, Palette.gray700 |> Palette.toFontColor ]
            (Element.text <| day)


header : RenderConfig -> (Msg -> msg) -> Property msg -> Element msg
header renderConfig  externalMsg model =
    let
        label =
            Date.format "MMMM" model.current ++ " " ++ (String.fromInt <| Date.year model.current)
    in
    Element.row [ Element.width fill ]
        [ Button.fromIcon (Icon.chevronLeft "previous")
                |> Button.cmd (externalMsg PreviousMonth) Button.clear
                |> Button.withSize Size.small
                |> Button.renderElement renderConfig
                |> Element.el [alignLeft]
        ,
        label
        |> Text.subtitle1
            |> Text.withColor Palette.gray800
            |> Text.renderElement renderConfig
            |> Element.el [Element.centerX]
        , Button.fromIcon (Icon.chevronRight "previous")
                          |> Button.cmd (externalMsg NextMonth) Button.clear
                          |> Button.withSize Size.small
                          |> Button.renderElement renderConfig
                          |> Element.el [alignRight]
        ]


weekHeader : List String
weekHeader =
    [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]


daysInTheView : Int
daysInTheView =
    42


weekDayOfFirstDay : Date -> Int
weekDayOfFirstDay date =
    let
        firstDay =
            Date.fromCalendarDate (Date.year date) (Date.month date) 1
    in
    Date.weekdayNumber firstDay


dateDaysFromPreviousMonth : Date -> List DayItem
dateDaysFromPreviousMonth date =
    let
        firstWeekDayMonth =
            weekDayOfFirstDay date
    in
    if firstWeekDayMonth == 1 then
        []

    else
        let
            year =
                Date.year date

            month =
                Date.month date

            first =
                Date.fromCalendarDate year month 1

            start =
                Date.add Days (-firstWeekDayMonth + 1) first

            until =
                Date.add Days -1 first

            dates =
                Date.range Day 1 start until ++ List.singleton until
        in
        List.map
            (\day ->
                { date = day
                , isCurrentMonth = False
                }
            )
            dates


dateDaysFromNextMonth : Int -> Date -> List DayItem
dateDaysFromNextMonth currentSize dayInCurrentMonth =
    if currentSize == daysInTheView then
        []

    else
        let
            nextMonth =
                Date.add Months 1 dayInCurrentMonth

            start =
                Date.fromCalendarDate (Date.year nextMonth) (Date.month nextMonth) 1

            until =
                Date.fromCalendarDate (Date.year nextMonth) (Date.month nextMonth) (daysInTheView - currentSize)

            dates =
                Date.range Day 1 start until ++ List.singleton until
        in
        List.map
            (\day ->
                { date = day
                , isCurrentMonth = False
                }
            )
            dates


datesOfTheMonth : Date -> List DayItem
datesOfTheMonth day =
    let
        year =
            Date.year day

        month =
            Date.month day

        start =
            Date.fromCalendarDate year month 1

        until =
            Date.fromCalendarDate year month <| daysOfMonth month year

        dates =
            Date.range Day 1 start until ++ [ Date.fromCalendarDate year month <| daysOfMonth month year ]
    in
    List.map
        (\date ->
            { date = date
            , isCurrentMonth = True
            }
        )
        dates


isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


daysOfMonth : Date.Month -> Int -> Int
daysOfMonth month year =
    case month of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear year then
                29

            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


split : Int -> List a -> ( List a, List a )
split i xs =
    ( List.take i xs, List.drop i xs )

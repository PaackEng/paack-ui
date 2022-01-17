module UI.DatePicker exposing (..)

import Date exposing (Date, Interval(..), Month, Unit(..))
import Element exposing (Element, alignLeft, alignRight, fill, fillPortion, maximum, minimum, padding, px, rgb, spacing, width)
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Task
import Time exposing (Month(..))
import UI.Button as Button
import UI.Effects as Effects exposing (Effects)
import UI.Icon as Icon
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


defaultToday : Date
defaultToday =
    Date.fromCalendarDate 2022 Feb 11


type alias Property =
    { today : Date
    , current : Date
    , selected : Maybe Date
    }


type alias DayItem =
    { date : Date
    , isCurrentMonth : Bool
    }


type DatePicker
    = DatePicker Property


init : Maybe Date -> ( DatePicker, Effects Msg )
init selected =
    ( DatePicker { today = defaultToday, selected = selected, current = defaultToday }, Effects.fromCmd <| Task.perform SetToday Date.today )



-- UPDATE


type Msg
    = Previous
    | Next
    | Selected Date
    | SetToday Date


type DateEvent
    = None
    | Picked Date
    | Current Date


update : Msg -> DatePicker -> ( DatePicker, DateEvent )
update msg (DatePicker ({ today, current, selected } as model)) =
    case msg of
        Previous ->
            let
                prev =
                    Date.add Months -1 current
            in
            ( DatePicker { model | current = prev }, Current prev )

        Next ->
            let
                next =
                    Date.add Months 1 current
            in
            ( DatePicker { model | current = next }, Current next )

        Selected date ->
            ( DatePicker { model | today = today, selected = Just date, current = current }, Picked date )

        SetToday date ->
            ( DatePicker { model | today = date, selected = Nothing, current = date }, None )



-- VIEW


datepicker : RenderConfig -> DatePicker -> Element Msg
datepicker renderConfig (DatePicker ({ today, current, selected } as model)) =
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
    Element.column [ width fill ]
        [ header renderConfig model
        , drawDateMatrix dates model
        ]


drawDateMatrix : List DayItem -> Property -> Element Msg
drawDateMatrix matrix ({ today, selected, current } as model) =
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
        , Border.width 1
        , Border.rounded 8
        , Palette.gray300 |> Palette.toBorderColor
        , Element.width
            (fill
                |> minimum 240
            )
        ]
        [ Element.row [ spacing 2, Element.centerX, Element.width fill ] (List.map drawCol weekHeader)
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model) first
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model) second
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model) third
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model) fourth
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model) fifth
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate model) sixth
        ]


drawDate : Property -> DayItem -> Element Msg
drawDate { today, selected, current } day =
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
        , Element.Events.onClick (Selected day.date)
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
                if day.isCurrentMonth then
                    Palette.blue800 |> Palette.toFontColor

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


drawCol : String -> Element Msg
drawCol day =
    Element.el [ Element.width (fillPortion 7), Element.centerX, Element.centerY ] <|
        Element.el [ Element.centerX, Font.size 12, Palette.gray700 |> Palette.toFontColor ]
            (Element.text <| day)


header : RenderConfig -> Property -> Element Msg
header renderConfig model =
    let
        label =
            Date.format "MMMM" model.current ++ " " ++ (String.fromInt <| Date.year model.current)
    in
    Element.row [ Element.width fill ]
        [ Button.fromIcon (Icon.chevronLeft "previous")
                |> Button.cmd Previous Button.clear
                |> Button.renderElement renderConfig

        ,
        label
        |> Text.subtitle1
            |> Text.withColor Palette.gray800
            |> Text.renderElement renderConfig
        , Button.fromIcon (Icon.chevronLeft "previous")
                          |> Button.cmd Next Button.clear
                          |> Button.renderElement renderConfig
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

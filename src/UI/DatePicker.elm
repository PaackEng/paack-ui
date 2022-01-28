module UI.DatePicker exposing (DateEvent(..), DatePicker, Msg, datepicker, init, update)

import Date exposing (Date, Interval(..), Unit(..))
import Element exposing (Element, alignLeft, alignRight, fill, fillPortion, minimum, px, spacing, width)
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Time
import UI.Button as Button
import UI.Icon as Icon
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text


{-| The `UI.Datepicker` is a component for displaying a calendar.

User must specify da today date, the date selected to construct it.


# Building

@docs DatePicker, datepicker


# Content

@docs init, update

-}
type alias Property msg =
    { today : Date
    , current : Date
    , selected : Maybe Date
    , toExternal : Msg -> msg
    }


{-| The properties of every single day.
-}
type alias DayItem =
    { date : Date
    , isCurrentMonth : Bool
    }


{-| The `DatePicker msg` type is used for describing the component for later
rendering.
-}
type DatePicker msg
    = DatePicker (Property msg)


{-| The initial configuration, receive a time zone, the today posix time, a possible date to select and an external function
to map the Msg.
-}
init : Time.Zone -> Time.Posix -> Maybe Date -> (Msg -> msg) -> DatePicker msg
init timeZone today selected toExt =
    let
        todayDate =
            Date.fromPosix timeZone today
    in
    DatePicker
        { today = todayDate
        , selected = selected
        , current =
            case selected of
                Just selected_ ->
                    selected_

                Nothing ->
                    todayDate
        , toExternal = toExt
        }



-- UPDATE


{-| Contains a pre-defined datapicker's change.
-}
type Msg
    = PreviousMonth
    | NextMonth
    | Selected Date


{-| Contains the data events generated from the picker.
-}
type DateEvent
    = Picked Date
    | Current Date


{-| The update function.
-}
update : Msg -> DatePicker msg -> ( DatePicker msg, DateEvent )
update msg (DatePicker ({ current } as model)) =
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
            ( DatePicker { model | selected = Just date }, Picked date )



-- VIEW


{-| Show the datapicker.
-}
datepicker : RenderConfig -> DatePicker msg -> Element msg
datepicker renderConfig (DatePicker ({ toExternal } as model)) =
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
        [ header renderConfig toExternal model
        , drawDateMatrix renderConfig dates model
        ]



-- Internals


{-| Draw the matrix of the days.
-}
drawDateMatrix : RenderConfig -> List DayItem -> Property msg -> Element msg
drawDateMatrix renderConfig matrix ({ toExternal } as model) =
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

        calendarWidth =
            if RenderConfig.isMobile renderConfig then
                fill |> minimum 240

            else
                fill |> minimum 240

        row =
            Element.row [ spacing 2, Element.centerX, Element.width fill ]

        head =
            row <| List.map drawDayName <| weekHeader renderConfig

        tail =
            [ first, second, third, fourth, fifth, sixth ]
                |> List.map (\days -> row <| List.map (drawDate renderConfig model toExternal) days)
    in
    head
        :: tail
        |> Element.column
            [ spacing 2
            , Element.width calendarWidth
            ]


{-| Draw a single day.
-}
drawDate : RenderConfig -> Property msg -> (Msg -> msg) -> DayItem -> Element msg
drawDate renderConfig { today, selected } externalToMsg day =
    let
        isSelected =
            case selected of
                Just date ->
                    date == day.date

                Nothing ->
                    False

        ( cellHeight, fontSize ) =
            if RenderConfig.isMobile renderConfig then
                ( px 44, 20 )

            else
                ( px 32, 16 )
    in
    Element.el
        [ Element.width (fillPortion 7)
        , Element.centerX
        , if isSelected then
            Palette.blue700 |> Palette.toBackgroundColor

          else
            Palette.genericWhite |> Palette.toBackgroundColor
        , Element.mouseOver
            [ if isSelected then
                Palette.blue700 |> Palette.toBackgroundColor

              else
                Palette.gray200 |> Palette.toBackgroundColor
            ]
        , Element.Events.onClick <| externalToMsg <| Selected day.date
        , if day.date == today then
            Element.inFront (Element.el [ Element.centerX, Element.alignBottom ] (Element.text "."))

          else
            Element.inFront Element.none
        , Border.rounded 6
        ]
    <|
        Element.column
            [ Element.height cellHeight
            , Element.centerX
            , if day.isCurrentMonth && not isSelected then
                Palette.blue800 |> Palette.toFontColor

              else if day.isCurrentMonth && isSelected then
                Palette.genericWhite |> Palette.toFontColor

              else
                Palette.gray500 |> Palette.toFontColor
            , Font.size fontSize
            ]
            [ Element.el
                [ Element.centerY
                ]
              <|
                Element.text <|
                    String.fromInt (Date.day day.date)
            ]


{-| Draw the name of the day.
-}
drawDayName : String -> Element msg
drawDayName day =
    Element.el [ Element.width (fillPortion 7), Element.centerX, Element.centerY ] <|
        Element.el [ Element.centerX, Font.size 12, Palette.gray700 |> Palette.toFontColor ]
            (Element.text <| day)


{-| Draw the header of the calendar.
-}
header : RenderConfig -> (Msg -> msg) -> Property msg -> Element msg
header renderConfig externalMsg model =
    let
        label =
            Date.format "MMMM" model.current ++ " " ++ (String.fromInt <| Date.year model.current)

        iconSize =
            if RenderConfig.isMobile renderConfig then
                Size.medium

            else
                Size.small
    in
    Element.row [ Element.width fill ]
        [ Button.fromIcon (Icon.chevronLeft "Previous Month")
            |> Button.cmd (externalMsg PreviousMonth) Button.clear
            |> Button.withSize iconSize
            |> Button.renderElement renderConfig
            |> Element.el [ alignLeft ]
        , label
            |> Text.subtitle1
            |> Text.withColor Palette.gray800
            |> Text.renderElement renderConfig
            |> Element.el [ Element.centerX ]
        , Button.fromIcon (Icon.chevronRight "Next Month")
            |> Button.cmd (externalMsg NextMonth) Button.clear
            |> Button.withSize iconSize
            |> Button.renderElement renderConfig
            |> Element.el [ alignRight ]
        ]


weekHeader : RenderConfig -> List String
weekHeader _ =
    [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]


{-| Numbers of the days to show in the calendar
-}
daysInTheView : Int
daysInTheView =
    42


{-| Return the week-day number of the 1th of the month
-}
weekDayOfFirstDay : Date -> Int
weekDayOfFirstDay date =
    let
        firstDay =
            Date.fromCalendarDate (Date.year date) (Date.month date) 1
    in
    Date.weekdayNumber firstDay


{-| Return the days to show from the previous month
-}
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


{-| Return the days to show from the next month
-}
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


{-| The days of the date month
-}
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

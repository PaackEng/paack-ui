module UI.DatePicker exposing
    ( DatePicker, singleDatePicker
    , withTodaysMark
    , renderElement
    , Model, init, update
    , Msg
    )

{-| The `UI.DatePicker` is a component for displaying a calendar.

User must specify da today date, the date selected to construct it.


# Building

@docs DatePicker, singleDatePicker


# Options

@docs withTodaysMark


# Rendering

@docs renderElement


# Model management

@docs Model, init, update

-}

import Date exposing (Date, Interval(..), Unit(..))
import Element exposing (Element, alignLeft, alignRight, fill, fillPortion, minimum, px, spacing, width)
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Time
import UI.Button as Button
import UI.Effects as Effects exposing (Effects)
import UI.Icon as Icon
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text


{-| The Model holding current viewing month and year.
-}
type Model
    = Model { current : Date }


{-| The `DatePicker msg` type is used for describing the component for later
rendering.
-}
type DatePicker msg
    = DatePicker (Property msg) Options


type alias Property msg =
    { toExternalMsg : Msg -> msg
    , onSelectMsg : Date -> msg
    , model : { current : Date }
    }


type alias Options =
    { today : Maybe Date
    , selected : Maybe Date
    }


{-| The properties of every single day.
-}
type alias DayItem =
    { date : Date
    , isCurrentMonth : Bool
    }


{-| The initial configuration, receives an initial date for focusing its month and year.

NOTE: This does not selects the said date, just start by displaying its month.

-}
init : Date -> Model
init startsWith =
    Model { current = startsWith }



-- UPDATE


{-| Contains a pre-defined datapicker's change.
-}
type Msg
    = PreviousMonth
    | NextMonth


{-| The update function.
-}
update : Msg -> Model -> ( Model, Effects msg )
update msg (Model ({ current } as model)) =
    case msg of
        PreviousMonth ->
            ( Model { model | current = Date.add Months -1 current }
            , Effects.none
            )

        NextMonth ->
            ( Model { model | current = Date.add Months 1 current }
            , Effects.none
            )



-- VIEW


singleDatePicker :
    { toExternalMsg : Msg -> msg
    , onSelectMsg : Date -> msg
    }
    -> Model
    -> Maybe Date
    -> DatePicker msg
singleDatePicker { toExternalMsg, onSelectMsg } (Model model) selected =
    DatePicker
        { toExternalMsg = toExternalMsg
        , onSelectMsg = onSelectMsg
        , model = model
        }
        { today = Nothing
        , selected = selected
        }


withTodaysMark : Date -> DatePicker msg -> DatePicker msg
withTodaysMark today (DatePicker prop opt) =
    DatePicker prop { opt | today = Just today }


{-| Show the datapicker.
-}
renderElement : RenderConfig -> DatePicker msg -> Element msg
renderElement renderConfig (DatePicker { toExternalMsg, onSelectMsg, model } options) =
    let
        numberOfDays =
            daysInMonth
                (Date.year model.current)
                (Date.month model.current)

        fromPreviousMonth =
            dateDaysFromPreviousMonth model.current

        dates =
            fromPreviousMonth
                ++ datesOfTheMonth model.current
                ++ dateDaysFromNextMonth (List.length fromPreviousMonth + numberOfDays) model.current
    in
    Element.column [ width fill ]
        [ monthPicker renderConfig toExternalMsg model
        , calendar renderConfig onSelectMsg options dates
        ]



-- Internals


calendar : RenderConfig -> (Date -> msg) -> Options -> List DayItem -> Element msg
calendar renderConfig onSelectMsg options dates =
    let
        ( first, firstRest ) =
            take7 dates

        ( second, secondRest ) =
            take7 firstRest

        ( third, thirdRest ) =
            take7 secondRest

        ( fourth, fourthRest ) =
            take7 thirdRest

        ( fifth, sixth ) =
            take7 fourthRest

        calendarWidth =
            fill |> minimum 240

        row =
            Element.row [ spacing 2, Element.centerX, Element.width fill ]

        head =
            row <| List.map dayOfTheWeek <| weekHeader renderConfig

        tail =
            List.map
                (List.map (singleDay renderConfig options onSelectMsg) >> row)
                [ first, second, third, fourth, fifth, sixth ]
    in
    head
        :: tail
        |> Element.column
            [ spacing 2
            , Element.width calendarWidth
            ]


singleDay : RenderConfig -> Options -> (Date -> msg) -> DayItem -> Element msg
singleDay renderConfig { today, selected } onSelectMsg day =
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

        textColor =
            if day.isCurrentMonth then
                if isSelected then
                    Palette.genericWhite

                else
                    Palette.blue800

            else
                Palette.gray500
    in
    Date.day day.date
        |> String.fromInt
        |> Element.text
        |> Element.el
            [ Element.centerY
            ]
        |> Element.el
            [ Element.height cellHeight
            , Element.centerX
            , Palette.toFontColor textColor
            , Font.size fontSize
            ]
        |> Element.el
            [ Element.width (fillPortion 1)
            , Element.centerX
            , Palette.toBackgroundColor <|
                if isSelected then
                    Palette.blue700

                else
                    Palette.genericWhite
            , Element.mouseOver
                [ Palette.toBackgroundColor <|
                    if isSelected then
                        Palette.blue700

                    else
                        Palette.gray200
                ]
            , Element.Events.onClick <| onSelectMsg day.date
            , if Just day.date == today then
                Element.inFront (todaysMarkElement textColor)

              else
                Element.inFront Element.none
            , Border.rounded 6
            ]


todaysMarkElement : Palette.Color -> Element msg
todaysMarkElement color =
    Element.el
        [ Element.centerX
        , Element.alignBottom
        , Palette.toFontColor color
        ]
        (Element.text ".")


dayOfTheWeek : String -> Element msg
dayOfTheWeek =
    Element.el
        [ Element.width (fillPortion 1)
        , Element.centerY
        , Font.center
        , Font.size 12
        , Palette.toFontColor Palette.gray700
        ]
        << Element.text


{-| Draw the header of the calendar.
-}
monthPicker : RenderConfig -> (Msg -> msg) -> { current : Date } -> Element msg
monthPicker renderConfig externalMsg model =
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
    if firstWeekDayMonth /= 1 then
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

    else
        []


{-| Return the days to show from the next month
-}
dateDaysFromNextMonth : Int -> Date -> List DayItem
dateDaysFromNextMonth currentSize dayInCurrentMonth =
    if currentSize /= daysInTheView then
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

    else
        []


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

        daysInMonth_ =
            daysInMonth year month

        until =
            Date.fromCalendarDate year month daysInMonth_

        dates =
            Date.range Day 1 start until
                ++ [ Date.fromCalendarDate year month daysInMonth_ ]
    in
    List.map
        (\date ->
            { date = date
            , isCurrentMonth = True
            }
        )
        dates


{-| From: <https://github.com/justinmimbs/date/blob/4.0.0/src/Date.elm#L156>
-}
isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


{-| From: <https://github.com/justinmimbs/date/blob/4.0.0/src/Date.elm#L1469>
-}
daysInMonth : Int -> Date.Month -> Int
daysInMonth year month =
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


take7 : List a -> ( List a, List a )
take7 xs =
    case xs of
        a :: b :: c :: d :: e :: f :: g :: tail ->
            ( [ a, b, c, d, e, f, g ], tail )

        _ ->
            ( xs, [] )

module UI.DatePicker exposing
    ( DatePicker, singleDatePicker
    , withTodaysMark, withRangeLimits
    , renderElement
    , Model, init, Msg, update
    )

{-| The `UI.DatePicker` is a component that displays a calendar for the user to pick a date.

        DatePicker.singleDatePicker
            { toExternalMsg = Msg.ToDatePicker
            , onSelectMsg = Msg.SelectDate
            }
            model.datePickerModel
            model.maybeSelectedDate
            |> DatePicker.withTodaysMark appConfig.timeNow
            |> DatePicker.renderElement appConfig.renderConfig


# Building

@docs DatePicker, singleDatePicker


# Options

@docs withTodaysMark, withRangeLimits


# Rendering

@docs renderElement


# Model management

@docs Model, init, Msg, update

-}

import Calendar exposing (Date)
import Element exposing (Element, alignLeft, alignRight, fill, fillPortion, minimum, px, spacing, width)
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Time
import UI.Button as Button
import UI.Effects as Effects exposing (Effects)
import UI.Icon as Icon
import UI.Internal.RenderConfig as RenderConfig
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text


{-| The Model holding current viewing month and year.
-}
type Model
    = Model { current : Date }


{-| The `DatePicker msg` type is used for describing the component for later rendering.
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
    , lowerLimit : Maybe Date
    , topperLimit : Maybe Date
    }


{-| The properties of every single day.
-}
type alias DayItem =
    { date : Date
    , isCurrentMonth : Bool
    }


{-| Creates the starting Model. It receives an initial date for focusing its month and year.

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
            ( Model { model | current = Calendar.decrementMonth current }
            , Effects.none
            )

        NextMonth ->
            ( Model { model | current = Calendar.incrementMonth current }
            , Effects.none
            )



-- VIEW


{-| Allows picking a single date

    singleDatePicker
        { toExternalMsg = Msg.ToDatePicker
        , onSelectMsg = Msg.SelectDate
        }
        model.datePickerModel
        model.maybeSelectedDate

-}
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
        , lowerLimit = Nothing
        , topperLimit = Nothing
        }


{-| Mark today's day with a single blue dot below it.
-}
withTodaysMark : Date -> DatePicker msg -> DatePicker msg
withTodaysMark today (DatePicker prop opt) =
    DatePicker prop { opt | today = Just today }


{-| Limits the days that can be selected to a specific range.

NOTE: This function does not validate/change the current viewing month or selected date.

-}
withRangeLimits : Maybe Date -> Maybe Date -> DatePicker msg -> DatePicker msg
withRangeLimits maybeFirst maybeSecond (DatePicker prop opt) =
    case ( maybeFirst, maybeSecond ) of
        ( Just first, Just second ) ->
            if Calendar.compare first second == GT then
                DatePicker prop { opt | lowerLimit = maybeSecond, topperLimit = maybeFirst }

            else
                DatePicker prop { opt | lowerLimit = maybeFirst, topperLimit = maybeSecond }

        _ ->
            DatePicker prop { opt | lowerLimit = maybeFirst, topperLimit = maybeSecond }


{-| Show the datapicker.
-}
renderElement : RenderConfig -> DatePicker msg -> Element msg
renderElement renderConfig (DatePicker { toExternalMsg, onSelectMsg, model } options) =
    let
        firstDay =
            untilIsFirstDay model.current

        leftDates =
            dateDaysFromPreviousMonth firstDay
                ++ datesOfTheMonth model.current

        dates =
            leftDates ++ dateDaysFromNextMonth firstDay (List.length leftDates)
    in
    Element.column [ width fill ]
        [ monthPicker renderConfig toExternalMsg options model firstDay
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
singleDay renderConfig { today, selected, lowerLimit, topperLimit } onSelectMsg day =
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
            if day.isCurrentMonth && enabled then
                if isSelected then
                    Palette.genericWhite

                else
                    Palette.blue800

            else
                Palette.gray500

        attrs =
            [ Element.width (fillPortion 1)
            , Element.centerX
            , Palette.toBackgroundColor <|
                if isSelected then
                    Palette.blue700

                else
                    Palette.genericWhite
            , if Just day.date == today then
                Element.inFront (todaysMarkElement textColor)

              else
                Element.inFront Element.none
            , Border.rounded 6
            ]

        enabled =
            inRange lowerLimit topperLimit day.date

        clickableAttrs =
            if enabled then
                Element.mouseOver
                    [ Palette.toBackgroundColor <|
                        if isSelected then
                            Palette.blue700

                        else
                            Palette.gray200
                    ]
                    :: (Element.Events.onClick <| onSelectMsg day.date)
                    :: Element.pointer
                    :: attrs

            else
                attrs
    in
    Calendar.getDay day.date
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
        |> Element.el clickableAttrs


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
monthPicker : RenderConfig -> (Msg -> msg) -> Options -> { current : Date } -> Date -> Element msg
monthPicker renderConfig externalMsg options model firstDay =
    let
        label =
            String.join " "
                [ monthName renderConfig <| Calendar.getMonth model.current
                , String.fromInt <| Calendar.getYear model.current
                ]

        iconSize =
            if RenderConfig.isMobile renderConfig then
                Size.medium

            else
                Size.small

        disablePrev =
            case options.lowerLimit of
                Just value ->
                    Calendar.compare (Calendar.decrementDay firstDay) value == LT

                Nothing ->
                    False

        disableNext =
            case options.topperLimit of
                Just value ->
                    Calendar.compare (Calendar.incrementMonth firstDay) value == GT

                Nothing ->
                    False

        terms =
            RenderConfig.localeTerms renderConfig
                |> .calendar
    in
    Element.row [ Element.width fill ]
        [ terms.prevMonth
            |> Icon.chevronLeft
            |> Button.fromIcon
            |> Button.cmd (externalMsg PreviousMonth) Button.clear
            |> Button.withSize iconSize
            |> Button.withDisabledIf disablePrev
            |> Button.renderElement renderConfig
            |> Element.el [ alignLeft ]
        , label
            |> Text.subtitle1
            |> Text.withColor Palette.gray800
            |> Text.renderElement renderConfig
            |> Element.el [ Element.centerX ]
        , terms.nextMonth
            |> Icon.chevronRight
            |> Button.fromIcon
            |> Button.cmd (externalMsg NextMonth) Button.clear
            |> Button.withSize iconSize
            |> Button.withDisabledIf disableNext
            |> Button.renderElement renderConfig
            |> Element.el [ alignRight ]
        ]


weekHeader : RenderConfig -> List String
weekHeader renderConfig =
    let
        { mon, tue, wed, thu, fri, sat, sun } =
            RenderConfig.localeTerms renderConfig
                |> .calendar
    in
    [ mon, tue, wed, thu, fri, sat, sun ]


{-| Numbers of the days to show in the calendar
-}
daysInTheView : Int
daysInTheView =
    42


{-| Return the days to show from the previous month
-}
dateDaysFromPreviousMonth : Date -> List DayItem
dateDaysFromPreviousMonth firstDay =
    if Calendar.getWeekday firstDay /= Time.Mon then
        untilIsMonday (Calendar.decrementDay firstDay) []

    else
        []


{-| Return the days to show from the next month
-}
dateDaysFromNextMonth : Date -> Int -> List DayItem
dateDaysFromNextMonth firstDay currentSize =
    if currentSize /= daysInTheView then
        let
            start =
                Calendar.incrementMonth firstDay

            end =
                incrementUntilIsDay (daysInTheView - currentSize) start

            dates =
                Calendar.getDateRange start end
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
datesOfTheMonth currentDate =
    currentDate
        |> Calendar.getDatesInMonth
        |> List.map
            (\date ->
                { date = date
                , isCurrentMonth = True
                }
            )


take7 : List a -> ( List a, List a )
take7 xs =
    case xs of
        a :: b :: c :: d :: e :: f :: g :: tail ->
            ( [ a, b, c, d, e, f, g ], tail )

        _ ->
            ( xs, [] )


untilIsFirstDay : Date -> Date
untilIsFirstDay date =
    if Calendar.getDay date /= 1 then
        untilIsFirstDay (Calendar.decrementDay date)

    else
        date


incrementUntilIsDay : Int -> Date -> Date
incrementUntilIsDay expectedDay date =
    if Calendar.getDay date /= expectedDay then
        incrementUntilIsDay expectedDay (Calendar.incrementDay date)

    else
        date


untilIsMonday : Date -> List DayItem -> List DayItem
untilIsMonday date accu =
    if Calendar.getWeekday date /= Time.Mon then
        untilIsMonday (Calendar.decrementDay date) ({ date = date, isCurrentMonth = False } :: accu)

    else
        { date = date, isCurrentMonth = False } :: accu


inRange : Maybe Date -> Maybe Date -> Date -> Bool
inRange lowerLimit topperLimit day =
    let
        afterLower =
            case lowerLimit of
                Just value ->
                    Calendar.compare day value /= LT

                Nothing ->
                    True

        beforeTopper =
            case topperLimit of
                Just value ->
                    Calendar.compare day value /= GT

                Nothing ->
                    True
    in
    afterLower && beforeTopper


monthName : RenderConfig -> Time.Month -> String
monthName renderConfig month =
    let
        terms =
            RenderConfig.localeTerms renderConfig
                |> .calendar
    in
    case month of
        Time.Jan ->
            terms.jan

        Time.Feb ->
            terms.feb

        Time.Mar ->
            terms.mar

        Time.Apr ->
            terms.apr

        Time.May ->
            terms.may

        Time.Jun ->
            terms.jun

        Time.Jul ->
            terms.jul

        Time.Aug ->
            terms.aug

        Time.Sep ->
            terms.sep

        Time.Oct ->
            terms.oct

        Time.Nov ->
            terms.nov

        Time.Dec ->
            terms.dec

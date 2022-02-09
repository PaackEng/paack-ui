module DatePicker.Stories exposing (..)

import Calendar
import DatePicker.Model as StoryModel
import DatePicker.Msg as DatePickerMsg
import Element exposing (Element, spacing)
import Model exposing (Model)
import Msg as RootMsg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import Time
import UI.DatePicker as DatePicker exposing (DatePicker)
import UI.Internal.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, iconsSvgSprite, prettifyElmCode, storyWithModel)


update : RenderConfig -> DatePickerMsg.Msg -> StoryModel.Model -> Return RootMsg.Msg StoryModel.Model
update _ msg model =
    case msg of
        DatePickerMsg.ToDatePicker subMsg ->
            let
                ( newDatePicker, _ ) =
                    DatePicker.update subMsg model.datePicker
            in
            ( { model | datePicker = newDatePicker }
            , Cmd.none
            )

        DatePickerMsg.Select date ->
            ( { model | selected = Just date }
            , Cmd.none
            )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "DatePicker"
        [ basicCalendarStory renderConfig False
        , basicCalendarStory renderConfig True
        ]


basicCalendarStory : RenderConfig -> Bool -> ExplorerStory
basicCalendarStory cfg isLimited =
    storyWithModel
        ( if isLimited then
            "Limited"

          else
            "Basic"
        , calendarView cfg isLimited
        , { defaultWithMenu
            | code =
                prettifyElmCode <|
                    if isLimited then
                        limitedDatePickerCode

                    else
                        basicDatePickerCode
            , note = goToDocsCallToAction ""
          }
        )


calendarView : RenderConfig -> Bool -> Model -> Element Msg
calendarView renderConfig isLimited { datePickerStories } =
    Element.column [ spacing 15 ]
        [ iconsSvgSprite
        , case datePickerStories.selected of
            Just date ->
                String.join " "
                    [ "Selected date:"
                    , Calendar.getYear date
                        |> String.fromInt
                    , Calendar.getMonth date
                        |> Calendar.monthToInt
                        |> String.fromInt
                        |> String.padLeft 2 '0'
                    , Calendar.getDay date
                        |> String.fromInt
                        |> String.padLeft 2 '0'
                    ]
                    |> Text.body1
                    |> Text.renderElement renderConfig
                    |> Element.el [ Element.centerX ]

            Nothing ->
                Text.body1 "No date selected."
                    |> Text.renderElement renderConfig
                    |> Element.el [ Element.centerX ]
        , DatePicker.singleDatePicker
            { toExternalMsg = DatePickerMsg.ToDatePicker >> RootMsg.DatePickerStoriesMsg
            , onSelectMsg = DatePickerMsg.Select >> RootMsg.DatePickerStoriesMsg
            }
            datePickerStories.datePicker
            datePickerStories.selected
            |> limit isLimited
            |> DatePicker.renderElement renderConfig
        ]


limit : Bool -> DatePicker msg -> DatePicker msg
limit isLimited =
    if isLimited then
        DatePicker.withRangeLimits
            (Just <| Calendar.fromPosix <| Time.millisToPosix 1638489600000)
            (Just <| Calendar.fromPosix <| Time.millisToPosix 1646265600000)

    else
        identity


basicDatePickerCode : String
basicDatePickerCode =
    """
DatePicker.singleDatePicker
        { toExternalMsg = Msg.ToDatePicker
        , onSelectMsg = Msg.SelectDate
        }
        model.datePickerModel
        model.maybeSelectedDate
        |> DatePicker.renderElement renderConfig
"""


limitedDatePickerCode : String
limitedDatePickerCode =
    """
DatePicker.singleDatePicker
        { toExternalMsg = Msg.ToDatePicker
        , onSelectMsg = Msg.SelectDate
        }
        model.datePickerModel
        model.maybeSelectedDate
        |> DatePicker.withRangeLimits
            (Just <| Calendar.fromPosix <| Time.millisToPosix 1638489600000)
            (Just <| Calendar.fromPosix <| Time.millisToPosix 1646265600000)
        |> DatePicker.renderElement renderConfig
"""

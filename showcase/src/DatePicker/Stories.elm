module DatePicker.Stories exposing (..)

import Calendar
import DatePicker.Model as StoryModel
import DatePicker.Msg as DatePickerMsg
import Element exposing (Element, spacing)
import Model exposing (Model)
import Msg as RootMsg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import UI.DatePicker as DatePicker
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
        [ basicCalendarStory renderConfig ]


basicCalendarStory : RenderConfig -> ExplorerStory
basicCalendarStory cfg =
    storyWithModel
        ( "Basic"
        , calendarView cfg
        , { defaultWithMenu
            | code = prettifyElmCode basicDatePickerCode
            , note = goToDocsCallToAction ""
          }
        )


calendarView : RenderConfig -> Model -> Element Msg
calendarView renderConfig { datePickerStories } =
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
                Element.none
        , DatePicker.singleDatePicker
            { toExternalMsg = DatePickerMsg.ToDatePicker >> RootMsg.DatePickerStoriesMsg
            , onSelectMsg = DatePickerMsg.Select >> RootMsg.DatePickerStoriesMsg
            }
            datePickerStories.datePicker
            datePickerStories.selected
            |> DatePicker.renderElement renderConfig
        ]


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

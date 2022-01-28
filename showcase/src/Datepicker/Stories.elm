module Datepicker.Stories exposing (..)

import Date
import Datepicker.Model as Datepicker
import Datepicker.Msg as DatepickerMsg
import Element exposing (Element, spacing)
import Model exposing (Model)
import Msg as RootMsg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import UI.DatePicker as DatePicker exposing (DateEvent(..))
import UI.Internal.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, prettifyElmCode, storyWithModel)


update : RenderConfig -> DatepickerMsg.Msg -> Datepicker.Model -> Return RootMsg.Msg Datepicker.Model
update _ msg model =
    case msg of
        DatepickerMsg.ToDatePicker subMsg ->
            let
                ( picker, date ) =
                    DatePicker.update subMsg model.datepicker

                newModel =
                    case date of
                        Picked date_ ->
                            { model
                                | datepicker = picker
                                , selected = Just date_
                            }

                        _ ->
                            { model
                                | datepicker = picker
                            }
            in
            ( newModel
            , Cmd.none
            )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Datepicker"
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
calendarView renderConfig calendarStories =
    Element.column [ spacing 15 ]
        [ case calendarStories.datepickerStories.selected of
            Just date ->
                "Date selected "
                    ++ Date.toIsoString date
                    |> Text.body1
                    |> Text.renderElement renderConfig
                    |> Element.el [ Element.centerX ]

            Nothing ->
                Element.none
        , DatePicker.datepicker renderConfig calendarStories.datepickerStories.datepicker
        ]


basicDatePickerCode : String
basicDatePickerCode =
    """
DatePicker.init timeZone timeNow Nothing ToDatePicker
    """

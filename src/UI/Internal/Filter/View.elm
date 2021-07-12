module UI.Internal.Filter.View exposing (..)

import Element exposing (Element, fill, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html.Attributes as HtmlAttrs
import UI.Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.Colors as Colors
import UI.Internal.Filter.Sorter exposing (SortingDirection(..))
import UI.Internal.RenderConfig as RenderConfig
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Focus as Focus


type FilterSize
    = ExtraSmall
    | Medium


type FilterWidth
    = WidthFull
    | WidthShrink


type Header msg
    = Header
        { label : String, openMsg : msg }
        CommonOptions
        { sorting : Maybe SortingDirection
        , applied : Maybe { preview : String, discardMsg : msg }
        }


type Body msg
    = Body
        { label : String, closeMsg : msg }
        CommonOptions
        { sorting :
            Maybe
                { smaller : String
                , larger : String
                , ascendingSortMsg : msg
                , descendingSortMsg : msg
                }
        , options : List (Element msg)
        , buttons : List (Button msg)
        , closeMsg : msg
        }


type alias CommonOptions =
    { width : FilterWidth
    , size : FilterSize
    }


header : String -> msg -> Header msg
header label openMsg =
    Header
        { label = label, openMsg = openMsg }
        { width = WidthFull, size = Medium }
        { sorting = Nothing, applied = Nothing }


headerWithWidth : FilterWidth -> Header msg -> Header msg
headerWithWidth width (Header prop common options) =
    Header prop { common | width = width } options


headerWithSize : FilterSize -> Header msg -> Header msg
headerWithSize size (Header prop common options) =
    Header prop { common | size = size } options


headerWithSorting : Maybe SortingDirection -> Header msg -> Header msg
headerWithSorting sorting (Header prop common options) =
    Header prop common { options | sorting = sorting }


headerWithApplied :
    Maybe { preview : String, discardMsg : msg }
    -> Header msg
    -> Header msg
headerWithApplied applied (Header prop common options) =
    Header prop common { options | applied = applied }


extraSmall : FilterSize
extraSmall =
    ExtraSmall


headerToElement : RenderConfig -> Header msg -> Element msg
headerToElement renderConfig (Header { label, openMsg } common { sorting, applied }) =
    let
        terms =
            RenderConfig.localeTerms renderConfig

        { padding, fontSize, iconSize } =
            if common.size == Medium then
                { padding = { left = 10, bottom = 7, top = 9, right = 12 }
                , fontSize = 16
                , iconSize = 20
                }

            else
                { padding = { left = 8, bottom = 7, top = 7, right = 12 }
                , fontSize = 12
                , iconSize = 16
                }

        attrs =
            ARIA.toElementAttributes ARIA.roleButton
                ++ Focus.toElementAttributes
                    { onEnter = openMsg
                    , tabIndex = 0
                    , hasFocus = False
                    }
                ++ [ Element.width <|
                        if common.width == WidthFull then
                            fill

                        else
                            shrink
                   , Element.paddingEach padding
                   , Element.spacing 8
                   , Background.color baseColor
                   , Border.width 2
                   , Border.rounded 6
                   , Border.color baseColor
                   , Font.color Colors.navyBlue700
                   , Font.size fontSize
                   , Font.bold
                   , Element.focused
                        [ Border.color Colors.navyBlue600
                        ]
                   , Element.mouseOver
                        [ Background.color Colors.gray300
                        , Border.color Colors.gray300
                        ]
                   , Element.pointer
                   , Events.onClick openMsg
                   ]

        body =
            [ sortingIcon
            , Element.text label
            , preview
            , rightIcon
            ]

        sortingIcon =
            case sorting of
                Just SortIncreasing ->
                    terms.tables.sorting.increase
                        |> Icon.sortIncreasing
                        |> Icon.withCustomSize iconSize
                        |> Icon.renderElement renderConfig
                        |> Element.el [ Element.width shrink ]

                Just SortDecreasing ->
                    terms.tables.sorting.decrease
                        |> Icon.sortDecreasing
                        |> Icon.withCustomSize iconSize
                        |> Icon.renderElement renderConfig

                Nothing ->
                    Element.none

        { preview, rightIcon, baseColor } =
            case applied of
                Just appliedData ->
                    { preview = Element.text <| "(" ++ appliedData.preview ++ ")"
                    , rightIcon =
                        Icon.close "Discard"
                            |> Icon.withCustomSize iconSize
                            |> Icon.renderElement renderConfig
                            |> Element.el
                                -- REVIEW: Add ARIA + Focus
                                [ Element.alignRight
                                , Element.focused
                                    [ Border.color Colors.navyBlue600
                                    ]
                                , Element.pointer
                                , Events.onClick appliedData.discardMsg
                                ]
                    , baseColor = Colors.navyBlue200
                    }

                Nothing ->
                    { preview = Element.none
                    , rightIcon =
                        Icon.filter label
                            |> Icon.withCustomSize iconSize
                            |> Icon.renderElement renderConfig
                            |> Element.el [ Element.alignRight ]
                    , baseColor = Colors.gray200
                    }
    in
    Element.row attrs body

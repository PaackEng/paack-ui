module UI.Internal.Filter.View exposing
    ( Body
    , BodySorting
    , CommonOptions
    , FilterSize(..)
    , FilterWidth(..)
    , Header
    , body
    , bodyToElement
    , bodyWithRows
    , bodyWithSize
    , bodyWithSorting
    , bodyWithWidth
    , header
    , headerToElement
    , headerWithApplied
    , headerWithSize
    , headerWithSorting
    , headerWithWidth
    , sizeExtraSmall
    , sizeMedium
    )

import Element exposing (Attribute, Element, fill, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html.Attributes as HtmlAttrs
import UI.Button as Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.Colors as Colors
import UI.Internal.Filter.Sorter exposing (SortingDirection(..))
import UI.Internal.RenderConfig as RenderConfig
import UI.Internal.Utils.Element as Element
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element exposing (RectangleSides, zeroPadding)


type FilterSize
    = ExtraSmall
    | Medium


type FilterWidth
    = WidthFull


type alias CommonOptions =
    { width : FilterWidth
    , size : FilterSize
    }


sizeExtraSmall : FilterSize
sizeExtraSmall =
    ExtraSmall


sizeMedium : FilterSize
sizeMedium =
    Medium


{-| The filter header-state button
-}
type Header msg
    = Header
        { label : String, openMsg : msg }
        CommonOptions
        { sorting : Maybe SortingDirection
        , applied : Maybe { preview : String, clearMsg : msg }
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
    Maybe { preview : String, clearMsg : msg }
    -> Header msg
    -> Header msg
headerWithApplied applied (Header prop common options) =
    Header prop common { options | applied = applied }


headerToElement : RenderConfig -> Header msg -> Element msg
headerToElement renderConfig (Header { label, openMsg } common { sorting, applied }) =
    let
        terms =
            RenderConfig.localeTerms renderConfig

        { padding, fontSize, iconSize } =
            headerProportions common.size

        attrs =
            ARIA.toElementAttributes ARIA.roleButton
                ++ [ Element.width <|
                        if common.width == WidthFull then
                            fill

                        else
                            shrink
                   , Element.paddingEach padding
                   , Element.spacing 8
                   , Background.color baseColor
                   , Border.width 2
                   , roundedBorders
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
                   , Element.onIndividualClick openMsg
                   , Element.onEnterPressed openMsg
                   , Element.tabIndex 0
                   ]

        sortingIcon =
            headerSortingIcon renderConfig iconSize sorting

        { preview, rightIcon, baseColor } =
            case applied of
                Just appliedData ->
                    { preview = Element.text <| "(" ++ appliedData.preview ++ ")"
                    , rightIcon = headerClearIcon renderConfig appliedData.clearMsg iconSize
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
    Element.row attrs
        [ sortingIcon
        , Element.text label
        , preview
        , rightIcon
        ]


headerSortingIcon : RenderConfig -> Int -> Maybe SortingDirection -> Element msg
headerSortingIcon renderConfig iconSize sorting =
    case sorting of
        Just SortIncreasing ->
            RenderConfig.localeTerms renderConfig
                |> (.tables >> .sorting >> .increase)
                |> Icon.sortIncreasing
                |> Icon.withCustomSize iconSize
                |> Icon.renderElement renderConfig
                |> Element.el [ Element.width shrink ]

        Just SortDecreasing ->
            RenderConfig.localeTerms renderConfig
                |> (.tables >> .sorting >> .decrease)
                |> Icon.sortDecreasing
                |> Icon.withCustomSize iconSize
                |> Icon.renderElement renderConfig
                |> Element.el [ Element.width shrink ]

        Nothing ->
            Element.none


headerClearIcon : RenderConfig -> msg -> Int -> Element msg
headerClearIcon renderConfig clearMsg iconSize =
    Icon.close "Discard"
        |> Icon.withCustomSize iconSize
        |> Icon.renderElement renderConfig
        |> Element.el
            (ARIA.toElementAttributes ARIA.roleButton
                ++ [ Element.alignRight
                   , Element.pointer
                   , Element.onIndividualClick clearMsg
                   , Element.onEnterPressed clearMsg
                   , Element.tabIndex 0
                   , Border.color (Element.rgba 1 1 1 0)
                   , Border.width 2
                   , Element.focused [ Border.color Colors.navyBlue600 ]
                   ]
            )


headerProportions :
    FilterSize
    ->
        { padding : RectangleSides
        , fontSize : Int
        , iconSize : Int
        }
headerProportions size =
    case size of
        Medium ->
            { padding = { left = 10, bottom = 7, top = 9, right = 12 }
            , fontSize = 16
            , iconSize = 20
            }

        ExtraSmall ->
            { padding = { left = 8, bottom = 7, top = 7, right = 12 }
            , fontSize = 12
            , iconSize = 16
            }


{-| The filter dialog-state body
-}
type Body msg
    = Body
        { label : String, closeMsg : msg }
        CommonOptions
        { sorting :
            Maybe (BodySorting msg)
        , rows : List (Element msg)
        , buttons : List (Button msg)
        }


type alias BodySorting msg =
    { smaller : String
    , larger : String
    , ascendingSortMsg : msg
    , descendingSortMsg : msg
    , clearSortMsg : msg
    , applied : Maybe SortingDirection
    }


body : String -> msg -> Body msg
body label closeMsg =
    Body
        { label = label, closeMsg = closeMsg }
        { width = WidthFull, size = Medium }
        { sorting = Nothing, rows = [], buttons = [] }


bodyWithWidth : FilterWidth -> Body msg -> Body msg
bodyWithWidth width (Body prop common options) =
    Body prop { common | width = width } options


bodyWithSize : FilterSize -> Body msg -> Body msg
bodyWithSize size (Body prop common options) =
    Body prop { common | size = size } options


bodyWithSorting : BodySorting msg -> Body msg -> Body msg
bodyWithSorting sorting (Body prop common options) =
    Body prop common { options | sorting = Just sorting }


bodyWithRows : List (Element msg) -> Body msg -> Body msg
bodyWithRows rows (Body prop common options) =
    Body prop common { options | rows = rows }


bodyToElement : RenderConfig -> Body msg -> Element msg
bodyToElement renderConfig (Body { label, closeMsg } common { sorting, rows, buttons }) =
    let
        attrs =
            [ Element.zIndex 9
            , Element.alignTop
            , Element.tuplesToStyles ( "min-width", "100%" )
            , Element.tuplesToStyles ( "width", "min-content" )
            , Colors.mainBackground
            , Border.shadow
                { offset = ( 0, 6 )
                , size = 0
                , blur = 20
                , color = Element.rgba 0 0 0 0.5
                }
            , roundedBorders
            ]

        bodyRows =
            [ bodyHeader renderConfig closeMsg label common.size
            , bodySorting renderConfig sorting
            ]
                ++ rows
                ++ bodyButtons renderConfig buttons
    in
    Element.column attrs
        bodyRows
        |> Element.clickElsewhereToLeave closeMsg []


bodyHeader : RenderConfig -> msg -> String -> FilterSize -> Element msg
bodyHeader renderConfig closeMsg label size =
    let
        { padding, fontSize, iconSize } =
            headerProportions size
    in
    Element.row
        [ Element.paddingEach padding
        , Element.width fill
        , Border.color Colors.gray300
        , Border.widthEach { zeroPadding | bottom = 1 }
        , Font.color Colors.navyBlue700
        , Font.size fontSize
        , Font.bold
        ]
        [ Element.text label
        , bodyCloseIcon renderConfig closeMsg iconSize
        ]


bodyCloseIcon : RenderConfig -> msg -> Int -> Element msg
bodyCloseIcon renderConfig closeMsg iconSize =
    Icon.close "Discard"
        |> Icon.withCustomSize iconSize
        |> Icon.renderElement renderConfig
        |> Element.el
            (ARIA.toElementAttributes ARIA.roleButton
                ++ [ Element.alignRight
                   , Element.pointer
                   , Events.onClick closeMsg
                   , Element.onEnterPressed closeMsg
                   , Element.tabIndex 0
                   ]
            )


bodyButtons : RenderConfig -> List (Button msg) -> List (Element msg)
bodyButtons renderConfig =
    let
        applier =
            Button.withWidth Button.widthFull
                >> Button.renderElement renderConfig
    in
    List.map applier


bodySorting : RenderConfig -> Maybe (BodySorting msg) -> Element msg
bodySorting renderConfig sorting =
    case sorting of
        Just {} ->
            Element.column [ Element.width fill ]
                []

        Nothing ->
            Element.none


roundedBorders : Attribute msg
roundedBorders =
    Border.rounded 6

module UI.Internal.Filter.View exposing
    ( Body
    , BodySorting
    , CommonOptions
    , FilterSize(..)
    , Header
    , body
    , bodyToElement
    , bodyWithButtons
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
import UI.Button as Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.Colors as Colors
import UI.Internal.Filter.Sorter exposing (SortingDirection(..))
import UI.Internal.RenderConfig as RenderConfig
import UI.Internal.Utils.Element as Element
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element exposing (RectangleSides, zeroPadding)


type FilterSize
    = ExtraSmall
    | Medium


type alias CommonOptions =
    { width : Element.Length
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
        { width = fill, size = Medium }
        { sorting = Nothing, applied = Nothing }


headerWithWidth : Element.Length -> Header msg -> Header msg
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
        { padding, fontSize, iconSize } =
            headerProportions common.size

        attrs =
            ARIA.toElementAttributes ARIA.roleButton
                ++ [ Element.width common.width
                   , Element.paddingEach padding
                   , Element.spacing 8
                   , Background.color baseColor
                   , Border.width 2
                   , roundedBorders
                   , Border.color baseColor
                   , Font.color Colors.navyBlue700
                   , Font.size fontSize
                   , Font.semiBold
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

        headerSortingIcon =
            sortingIcon renderConfig [ Element.width shrink ] iconSize sorting

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
        [ headerSortingIcon
        , Element.text label
        , preview
        , rightIcon
        ]


headerClearIcon : RenderConfig -> msg -> Int -> Element msg
headerClearIcon renderConfig clearMsg iconSize =
    RenderConfig.localeTerms renderConfig
        |> (.filters >> .clear)
        |> Icon.close
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
    -> { fontSize : Int, iconSize : Int, padding : RectangleSides }
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
        { width = fill, size = Medium }
        { sorting = Nothing, rows = [], buttons = [] }


bodyWithWidth : Element.Length -> Body msg -> Body msg
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


bodyWithButtons : List (Button msg) -> Body msg -> Body msg
bodyWithButtons buttons (Body prop common options) =
    Body prop common { options | buttons = buttons }


bodyToElement : RenderConfig -> Body msg -> Element msg
bodyToElement renderConfig (Body { label, closeMsg } common { sorting, rows, buttons }) =
    let
        width =
            if common.width == shrink then
                Element.tuplesToStyles ( "min-width", "100%" )

            else
                Element.width common.width

        attrs =
            [ Element.zIndex 9
            , Element.alignTop
            , Colors.mainBackground
            , Border.shadow
                { offset = ( 0, 6 )
                , size = 0
                , blur = 20
                , color = Element.rgba 0 0 0 0.5
                }
            , roundedBorders
            , width
            ]

        bodyRows =
            [ bodyHeader renderConfig common.size closeMsg label
            , bodySorting renderConfig common.size sorting
            , Element.column
                [ Element.width fill
                , Element.spacing 8
                , Element.padding 10
                ]
                rows
            , bodyButtons renderConfig common.size buttons
            ]
    in
    Element.column attrs
        bodyRows
        |> Element.clickElsewhereToLeave closeMsg []


bodyHeader : RenderConfig -> FilterSize -> msg -> String -> Element msg
bodyHeader renderConfig size closeMsg label =
    let
        { padding, fontSize, iconSize } =
            headerProportions size

        paddingWithBorders =
            { left = padding.left + 2
            , top = padding.top + 1
            , bottom = padding.bottom + 1
            , right = padding.right
            }
    in
    Element.row
        [ Element.paddingEach paddingWithBorders
        , Element.width fill
        , Border.color Colors.gray300
        , Border.widthEach { zeroPadding | bottom = 1 }
        , Font.color Colors.navyBlue700
        , Font.size fontSize
        , Font.semiBold
        ]
        [ Element.text label
        , bodyCloseIcon renderConfig closeMsg iconSize
        ]


bodyCloseIcon : RenderConfig -> msg -> Int -> Element msg
bodyCloseIcon renderConfig closeMsg iconSize =
    RenderConfig.localeTerms renderConfig
        |> (.filters >> .close)
        |> Icon.close
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


bodySorting : RenderConfig -> FilterSize -> Maybe (BodySorting msg) -> Element msg
bodySorting renderConfig size sorting =
    case sorting of
        Just sortingData ->
            let
                proportions =
                    bodySortingProportions size

                terms =
                    RenderConfig.localeTerms renderConfig
            in
            Element.column [ Element.width fill ]
                [ sortAs renderConfig
                    SortAscending
                    proportions
                    sortingData
                    terms.tables.sorting.ascending
                    sortingData.ascendingSortMsg
                , sortAs renderConfig
                    SortDescending
                    proportions
                    sortingData
                    terms.tables.sorting.descending
                    sortingData.descendingSortMsg
                ]

        Nothing ->
            Element.none


sortAs :
    RenderConfig
    -> SortingDirection
    -> { fontSize : Int, iconSize : Int, padding : RectangleSides }
    -> BodySorting msg
    -> String
    -> msg
    -> Element msg
sortAs renderConfig direction { fontSize, iconSize } sortingData label msg =
    let
        ( backgroundColor, allowClearingMsg ) =
            if sortingData.applied == Just direction then
                ( Background.color Colors.navyBlue200
                , sortingData.clearSortMsg
                )

            else
                ( Colors.mainBackground, msg )
    in
    Element.row
        [ Element.width fill
        , Font.color Colors.navyBlue700
        , Font.size fontSize
        , Font.medium
        , Element.paddingEach
            { top = 6, left = 12, right = 6, bottom = 6 }
        , Element.spacing 6
        , Border.color Colors.gray300
        , Border.widthEach { zeroPadding | bottom = 1 }
        , Events.onClick <| allowClearingMsg
        , Element.onEnterPressed <| allowClearingMsg
        , Element.tabIndex 0
        , backgroundColor
        , Element.pointer
        , Element.mouseOver
            [ Background.color Colors.gray300
            ]
        ]
        [ Element.paragraph []
            [ Element.text label
            , Element.text " ("
            , Element.text sortingData.smaller
            , Element.text " - "
            , Element.text sortingData.larger
            , Element.text ")"
            ]
        , sortingIcon renderConfig
            [ Element.alignRight
            ]
            iconSize
            (Just direction)
        ]


bodySortingProportions :
    FilterSize
    -> { fontSize : Int, iconSize : Int, padding : RectangleSides }
bodySortingProportions size =
    case size of
        Medium ->
            { padding = { left = 10, bottom = 7, top = 9, right = 12 }
            , fontSize = 10
            , iconSize = 10
            }

        ExtraSmall ->
            { padding = { left = 8, bottom = 7, top = 7, right = 12 }
            , fontSize = 10
            , iconSize = 10
            }


bodyButtons : RenderConfig -> FilterSize -> List (Button msg) -> Element msg
bodyButtons renderConfig size buttons =
    let
        buttonSize =
            case size of
                ExtraSmall ->
                    Size.extraSmall

                Medium ->
                    Size.small

        applier =
            Button.withWidth Button.widthFull
                >> Button.withSize buttonSize
                >> Button.renderElement renderConfig
    in
    Element.column
        [ Element.width fill
        , Element.spacing 8
        , Element.padding 12
        ]
        (List.map applier buttons)



{- Common -}


sortingIcon :
    RenderConfig
    -> List (Attribute msg)
    -> Int
    -> Maybe SortingDirection
    -> Element msg
sortingIcon renderConfig attrs iconSize sorting =
    case sorting of
        Just SortAscending ->
            RenderConfig.localeTerms renderConfig
                |> (.tables >> .sorting >> .ascending)
                |> Icon.sortIncreasing
                |> Icon.withCustomSize iconSize
                |> Icon.renderElement renderConfig
                |> Element.el attrs

        Just SortDescending ->
            RenderConfig.localeTerms renderConfig
                |> (.tables >> .sorting >> .descending)
                |> Icon.sortDecreasing
                |> Icon.withCustomSize iconSize
                |> Icon.renderElement renderConfig
                |> Element.el attrs

        Nothing ->
            Element.none


roundedBorders : Attribute msg
roundedBorders =
    Border.rounded 6

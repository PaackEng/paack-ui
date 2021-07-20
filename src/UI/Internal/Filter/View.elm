module UI.Internal.Filter.View exposing (..)

import Element exposing (Attribute, Element, fill, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Button as Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.Colors as Colors
import UI.Internal.Filter.Model as Model
import UI.Internal.Filter.Msg as Msg exposing (Msg)
import UI.Internal.Filter.Sorter exposing (SortingDirection(..))
import UI.Internal.RenderConfig as RenderConfig
import UI.Internal.Utils.Element as Element exposing (overlayZIndex)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size exposing (Size)
import UI.TextField as TextField
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element exposing (RectangleSides, zeroPadding)


type FilterSize
    = ExtraSmall
    | Medium


type alias CommonOptions =
    { width : Element.Length
    , size : FilterSize
    }


type alias Header msg =
    { label : String
    , openMsg : msg
    , common : CommonOptions
    , sorting : Maybe SortingDirection
    , applied : Maybe { preview : String, clearMsg : msg }
    }


type alias Body msg =
    { label : String
    , closeMsg : msg
    , common : CommonOptions
    , sorting : Maybe (BodySorting msg)
    , rows : List (Element msg)
    , buttons : List (Button msg)
    }


type alias BodySorting msg =
    { preview : Maybe { smaller : String, larger : String }
    , ascendingSortMsg : msg
    , descendingSortMsg : msg
    , clearSortMsg : msg
    , applied : Maybe SortingDirection
    }


headerToElement : RenderConfig -> Header msg -> Element msg
headerToElement renderConfig { label, openMsg, common, sorting, applied } =
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


bodyToElement : RenderConfig -> Body msg -> Element msg
bodyToElement renderConfig { label, closeMsg, common, sorting, rows, buttons } =
    let
        width =
            if common.width == shrink then
                Element.tuplesToStyles ( "min-width", "100%" )

            else
                Element.width common.width

        attrs =
            [ Element.zIndex (overlayZIndex + 1)
            , Element.alignTop
            , Colors.mainBackground
            , Border.shadow
                { offset = ( 0, 6 )
                , size = 0
                , blur = 20
                , color = Element.rgba 0 0 0 0.09
                }
            , Border.width 1
            , Border.color Colors.gray300
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
    Element.customOverlay closeMsg [] <| Element.column attrs bodyRows


bodyHeader : RenderConfig -> FilterSize -> msg -> String -> Element msg
bodyHeader renderConfig size closeMsg label =
    let
        { padding, fontSize, iconSize } =
            headerProportions size

        paddingWithBorders =
            { left = padding.left + 1
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

        labelParagraph =
            case ( sortingData.preview, direction ) of
                ( Just { smaller, larger }, SortAscending ) ->
                    [ label, " (", smaller, " - ", larger, ")" ]

                ( Just { smaller, larger }, SortDescending ) ->
                    [ label, " (", larger, " - ", smaller, ")" ]

                _ ->
                    [ label ]
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
        [ labelParagraph
            |> List.map Element.text
            |> Element.paragraph []
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
        applier =
            Button.withWidth Button.widthFull
                >> Button.withSize (sizeToElement size)
                >> Button.renderElement renderConfig
    in
    Element.column
        [ Element.width fill
        , Element.spacing 8
        , Element.padding 12
        ]
        (List.map applier buttons)


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


sizeToElement : FilterSize -> Size
sizeToElement size =
    case size of
        ExtraSmall ->
            Size.extraSmall

        Medium ->
            Size.small



{------- Default Presets ----------}


defaultButtons :
    { applyMsg : msg
    , clearMsg : msg
    , applyLabel : String
    , clearLabel : String
    }
    -> Model.Filter msg item
    -> List (Button msg)
defaultButtons { applyMsg, clearMsg, applyLabel, clearLabel } filter =
    let
        applyButton =
            applyLabel
                |> Button.fromLabel
                |> Button.cmd applyMsg Button.primary

        disabledApplyButton =
            applyLabel
                |> Button.fromLabel
                |> Button.disabled

        clearButton =
            clearLabel
                |> Button.fromLabel
                |> Button.cmd clearMsg Button.danger
    in
    case ( Model.isApplied filter, Model.isEdited filter ) of
        ( False, False ) ->
            [ disabledApplyButton ]

        ( False, True ) ->
            [ applyButton ]

        ( True, False ) ->
            [ clearButton ]

        ( True, True ) ->
            [ applyButton, clearButton ]


defaultSingleTextFilter :
    RenderConfig
    -> FilterSize
    -> String
    -> Model.Editable String
    -> List (Element Msg)
defaultSingleTextFilter renderConfig size label editable =
    editable
        |> Model.editableWithDefault ""
        |> TextField.singlelineText Msg.EditSingleText label
        |> TextField.withSize (sizeToElement size)
        |> TextField.withWidth TextField.widthFull
        |> TextField.withOnEnterPressed Msg.Apply
        |> TextField.renderElement renderConfig
        |> List.singleton

module UI.Table exposing
    ( CellWidth
    , HeaderRow
    , OptRow
    , Row
    , Table
    , cellFromButton
    , cellFromText
    , cellWidthEnd
    , cellWidthPixels
    , cellWidthPortion
    , cellWidthShrink
    , header
    , headersEnd
    , rowEnd
    , table
    , toEl
    , withCellsWidth
    , withResponsiveRows
    , withStaticRows
    , withWidth
    )

import Element exposing (Element, fill, fillPortion, px, shrink)
import Element.Border as Border
import UI.Button as Button exposing (Button)
import UI.Internal.Basics exposing (..)
import UI.Internal.NList as NList exposing (NList)
import UI.Internal.Palette as Palette
import UI.Internal.ToggableList as ToggableList
import UI.Internal.TypeNumbers as T
import UI.Palette as Palette exposing (brightnessMiddle, tonePrimary)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text exposing (Text)
import UI.Utils.Element exposing (zeroPadding)


type Table msg object columns
    = Table (Properties columns) (Options msg object columns)


type alias Properties columns =
    { headers : HeaderRow columns
    }


type alias Options msg object columns =
    { rows : Rows msg object columns
    , width : Element.Length
    }


type alias ResponsiveOptions msg object columns =
    { detailsShowLabel : String
    , detailsCollapseLabel : String
    , selectMsg : object -> msg
    , isSelected : object -> Bool
    , coverView : RenderConfig -> Palette.Color -> object -> Bool -> Element msg
    , toRows : object -> Row msg columns
    , items : List object
    }


type Rows msg object columns
    = StaticRows (List (Row msg columns))
    | ResponsiveRows (ResponsiveOptions msg object columns)


type alias Row msg columns =
    NList (Cell msg) columns


type alias HeaderRow columns =
    NList HeaderCell columns


type alias OptRow value columns =
    NList (Maybe value) columns


type alias HeaderCell =
    { title : String
    , width : CellWidth
    }


type Cell msg
    = CellText Text
    | CellButton (Button msg)


type CellWidth
    = WidthPixels Int
    | WidthPortion Int


headersEnd : HeaderRow T.Zero
headersEnd =
    NList.empty


rowEnd : Row msg T.Zero
rowEnd =
    NList.empty


header : String -> HeaderRow columns -> HeaderRow (T.Increase columns)
header head tail =
    NList.cons { title = head, width = WidthPortion 1 } tail


cellFromText : Text -> Row msg columns -> Row msg (T.Increase columns)
cellFromText text tail =
    NList.cons (CellText text) tail


cellFromButton : Button msg -> Row msg columns -> Row msg (T.Increase columns)
cellFromButton btn tail =
    NList.cons (CellButton btn) tail


table : HeaderRow columns -> Table msg object columns
table headers =
    Table { headers = headers } defaultOptions



-- Options


withStaticRows : List (Row msg columns) -> Table msg object columns -> Table msg object columns
withStaticRows rows (Table prop opt_) =
    Table prop { opt_ | rows = StaticRows rows }


withResponsiveRows : ResponsiveOptions msg object columns -> Table msg object columns -> Table msg object columns
withResponsiveRows responsiveOpt (Table prop opt_) =
    Table prop { opt_ | rows = ResponsiveRows responsiveOpt }


withWidth : Element.Length -> Table msg object columns -> Table msg object columns
withWidth width (Table prop opt_) =
    Table prop { opt_ | width = width }


withCellsWidth : OptRow CellWidth columns -> Table msg object columns -> Table msg object columns
withCellsWidth row (Table prop opt_) =
    let
        mergeWidth header_ maybeWidth =
            case maybeWidth of
                Just len ->
                    { header_ | width = len }

                Nothing ->
                    header_
    in
    Table { prop | headers = NList.map2 mergeWidth prop.headers row } opt_


cellWidthPortion : Int -> OptRow CellWidth columns -> OptRow CellWidth (T.Increase columns)
cellWidthPortion int accu =
    opt (WidthPortion int) accu


cellWidthPixels : Int -> OptRow CellWidth columns -> OptRow CellWidth (T.Increase columns)
cellWidthPixels int accu =
    opt (WidthPixels int) accu


cellWidthShrink : OptRow CellWidth columns -> OptRow CellWidth (T.Increase columns)
cellWidthShrink accu =
    optKeep accu


cellWidthEnd : OptRow CellWidth T.Zero
cellWidthEnd =
    optsEnd



-- Render


toEl : RenderConfig -> Table msg object columns -> Element msg
toEl cfg (Table { headers } { rows, width }) =
    case rows of
        ResponsiveRows responsiveOpt ->
            if RenderConfig.isMobile cfg then
                mobileView cfg headers responsiveOpt

            else
                responsiveOpt.items
                    |> List.map responsiveOpt.toRows
                    |> desktopView cfg True headers width

        StaticRows desktopRows ->
            desktopView cfg False headers width desktopRows



-- Internals


desktopView : RenderConfig -> Bool -> HeaderRow columns -> Element.Length -> List (Row msg columns) -> Element msg
desktopView cfg responsive headers width desktopRows =
    let
        rowRender row =
            row
                |> NList.map2 (cellRender cfg) headers
                |> NList.toList
                |> Element.row [ Element.spacing 4, Element.width fill ]
    in
    desktopRows
        |> List.map rowRender
        |> (::)
            (headers
                |> NList.map (headerRender cfg)
                |> NList.toList
                |> Element.row
                    [ Element.spacing 4
                    , Element.width fill
                    , Element.paddingEach { bottom = 9, top = 0, left = 0, right = 0 }
                    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                    , Border.color Palette.gray.lightest
                    ]
            )
        |> Element.column
            [ Element.spacing 16
            , Element.width width
            , Element.paddingEach <|
                ifThenElse responsive
                    { top = 20, left = 20, right = 20, bottom = 0 }
                    zeroPadding
            ]


mobileView : RenderConfig -> HeaderRow columns -> ResponsiveOptions msg object columns -> Element msg
mobileView renderConfig headers responsiveOpt =
    let
        rowMap object =
            object
                |> responsiveOpt.toRows
                |> NList.map2 Tuple.pair headers
                |> NList.toList

        details object =
            case rowMap object of
                [] ->
                    []

                _ :: tail ->
                    List.map
                        (\( cellHeader, cell ) ->
                            ( cellHeader.title
                            , cellRender renderConfig cellHeader cell
                            )
                        )
                        tail
    in
    ToggableList.view renderConfig
        { detailsShowLabel = responsiveOpt.detailsShowLabel
        , detailsCollapseLabel = responsiveOpt.detailsCollapseLabel
        , coverView = responsiveOpt.coverView
        , details = details
        , selectMsg = responsiveOpt.selectMsg
        , isSelected = responsiveOpt.isSelected
        }
        responsiveOpt.items


headerRender : RenderConfig -> HeaderCell -> Element msg
headerRender cfg { title, width } =
    Text.overline title
        |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
        |> Text.toEl cfg
        |> Element.el [ Element.width (widthToEl width) ]


cellRender : RenderConfig -> HeaderCell -> Cell msg -> Element msg
cellRender cfg { width } cell_ =
    case cell_ of
        CellText text ->
            text
                |> Text.toEl cfg
                |> Element.el [ Element.width (widthToEl width) ]

        CellButton button ->
            Button.toEl cfg button
                |> Element.el [ Element.width (widthToEl width) ]


widthToEl : CellWidth -> Element.Length
widthToEl width =
    case width of
        WidthPortion int ->
            fillPortion int

        WidthPixels int ->
            px int


defaultOptions : Options msg object columns
defaultOptions =
    { rows = StaticRows []
    , width = shrink
    }


optsEnd : OptRow value T.Zero
optsEnd =
    NList.empty


optKeep : OptRow value columns -> OptRow value (T.Increase columns)
optKeep tail =
    NList.cons Nothing tail


opt : value -> OptRow value columns -> OptRow value (T.Increase columns)
opt head tail =
    NList.cons (Just head) tail

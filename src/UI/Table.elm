module UI.Table exposing
    ( Events
    , Msg
    , State
    , default
    , initState
    , isItemHovered
    , isItemSelected
    , noEvents
    , toEl
    , update
    , withBody
    , withCustomWidth
    , withDisabled
    , withHeader
    , withOnRowClicked
    , withOnRowDoubleClicked
    , withOnRowSelected
    , withOnRowUnselected
    , withSelectableRows
    , withTextAlignedToCenter
    , withTextAlignedToLeft
    , withTextAlignedToRight
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Helpers exposing (ifThenElse)
import Return exposing (Return)
import ReturnExtra exposing (withCmd, withNoCmd)
import Task
import UI.Theme as Theme exposing (..)


type WidthType
    = SameWidth
    | CustomWidth (List Int)


type TextAlignment
    = Left
    | Center
    | Right


type InternalMsg item
    = RowHovered item
    | None
    | ClearHoverRow
    | ToggleRowSelection item
    | RowClicked item
    | RowDoubleClicked item


type Msg item
    = Msg (InternalMsg item)


type State item comparable
    = State (InternalState item comparable)


type alias InternalState item comparable =
    { tableRowHovered : Maybe item
    , tableRowSelected : Maybe item
    , toComparable : item -> comparable
    }


initState : (item -> comparable) -> State item comparable
initState toComparable =
    State
        { tableRowHovered = Nothing
        , tableRowSelected = Nothing
        , toComparable = toComparable
        }


update : Msg item -> Events msg item -> State item comparable -> Return msg (State item comparable)
update (Msg msg) (Events events) (State state) =
    case msg of
        RowHovered item ->
            State
                { state | tableRowHovered = Just item }
                |> withNoCmd

        ClearHoverRow ->
            State
                { state | tableRowHovered = Nothing }
                |> withNoCmd

        ToggleRowSelection newItem ->
            let
                newRowSelection =
                    case state.tableRowSelected of
                        Just oldItem ->
                            ifThenElse (state.toComparable oldItem == state.toComparable newItem)
                                Nothing
                                (Just newItem)

                        Nothing ->
                            Just newItem

                cmd =
                    case newRowSelection of
                        Just rowSelected ->
                            events.onRowSelected
                                |> Maybe.map (\event -> sendMsgToOutside (event rowSelected))
                                |> Maybe.withDefault Cmd.none

                        Nothing ->
                            Maybe.map2
                                (\event item -> sendMsgToOutside (event item))
                                events.onRowUnselected
                                state.tableRowSelected
                                |> Maybe.withDefault Cmd.none
            in
            State { state | tableRowSelected = newRowSelection }
                |> withCmd cmd

        RowClicked row ->
            let
                cmd =
                    events.onRowClicked
                        |> Maybe.map (\event -> sendMsgToOutside (event row))
                        |> Maybe.withDefault Cmd.none
            in
            State state |> withCmd cmd

        None ->
            State state |> withNoCmd

        RowDoubleClicked item ->
            let
                cmd =
                    case events.onRowDoubleClicked of
                        Just doubleClickMsg ->
                            sendMsgToOutside (doubleClickMsg item)

                        Nothing ->
                            Cmd.none
            in
            State state |> withCmd cmd



-- Table Builder


type alias Options msg item =
    { data : List item
    , toParentMsg : Msg item -> msg
    , body : List (item -> Element msg)
    , selectableRows : Bool
    , disabled : Bool
    , onDoubleClick : Maybe (item -> msg)
    , header : List (Element msg)
    , width : WidthType
    , textAlignment : TextAlignment
    }


type Table msg item
    = Table (Options msg item)


default : (Msg item -> msg) -> List item -> Table msg item
default toParentMsg data =
    Table
        { data = data
        , toParentMsg = toParentMsg
        , body = []
        , disabled = False
        , selectableRows = False
        , onDoubleClick = Nothing
        , header = []
        , width = SameWidth
        , textAlignment = Center
        }


withSelectableRows : Table msg item -> Table msg item
withSelectableRows (Table options) =
    Table
        { options | selectableRows = True && not options.disabled }


withDisabled : Bool -> Table msg item -> Table msg item
withDisabled bool (Table options) =
    Table
        { options | disabled = bool }


withHeader : List (Element msg) -> Table msg item -> Table msg item
withHeader header (Table options) =
    Table
        { options | header = header }


withBody : List (item -> Element msg) -> Table msg item -> Table msg item
withBody body (Table options) =
    Table
        { options | body = body }


withCustomWidth : List Int -> Table msg item -> Table msg item
withCustomWidth widths (Table options) =
    Table
        { options | width = CustomWidth widths }


withTextAlignedToLeft : Table msg item -> Table msg item
withTextAlignedToLeft (Table options) =
    Table
        { options | textAlignment = Left }


withTextAlignedToCenter : Table msg item -> Table msg item
withTextAlignedToCenter (Table options) =
    Table
        { options | textAlignment = Center }


withTextAlignedToRight : Table msg item -> Table msg item
withTextAlignedToRight (Table options) =
    Table
        { options | textAlignment = Right }


toEl : State item comparable -> Table msg item -> Element msg
toEl (State state) (Table options) =
    column [ width fill, height fill, centerX ] (headerView options :: bodyView state options)



-- Events Builder


type alias InternalEvents msg item =
    { onRowClicked : Maybe (item -> msg)
    , onRowSelected : Maybe (item -> msg)
    , onRowUnselected : Maybe (item -> msg)
    , onRowDoubleClicked : Maybe (item -> msg)
    }


type Events msg item
    = Events (InternalEvents msg item)


noEvents : Events msg item
noEvents =
    Events
        { onRowClicked = Nothing
        , onRowSelected = Nothing
        , onRowUnselected = Nothing
        , onRowDoubleClicked = Nothing
        }


withOnRowDoubleClicked : (item -> msg) -> Events msg item -> Events msg item
withOnRowDoubleClicked onRowDoubleClicked (Events events) =
    Events
        { events | onRowDoubleClicked = Just onRowDoubleClicked }


withOnRowClicked : (item -> msg) -> Events msg item -> Events msg item
withOnRowClicked onRowClicked (Events events) =
    Events
        { events | onRowClicked = Just onRowClicked }


withOnRowUnselected : (item -> msg) -> Events msg item -> Events msg item
withOnRowUnselected onRowUnselected (Events events) =
    Events
        { events | onRowUnselected = Just onRowUnselected }


withOnRowSelected : (item -> msg) -> Events msg item -> Events msg item
withOnRowSelected onRowSelected (Events events) =
    Events
        { events | onRowSelected = Just onRowSelected }



-- View


bodyView : InternalState item comparable -> Options msg item -> List (Element msg)
bodyView state options =
    if List.length options.body == 0 then
        []

    else
        List.map (rowView state options) options.data


rowView : InternalState item comparable -> Options msg item -> item -> Element msg
rowView state options item =
    let
        isMaybeEqual maybeField =
            maybeField
                |> Maybe.map (\m -> state.toComparable m == state.toComparable item)
                |> Maybe.withDefault False

        isHighligthed =
            isMaybeEqual state.tableRowHovered
                || (options.selectableRows && isMaybeEqual state.tableRowSelected)

        ( borderColor, backgroundColor, fontColor ) =
            if isHighligthed then
                ( Theme.gray3, Theme.gray4, Theme.black )

            else
                ( Theme.white, Theme.white, Theme.gray1 )

        rowClickMsg =
            if options.selectableRows then
                ToggleRowSelection

            else if options.disabled then
                always None

            else
                RowClicked

        rowAttrs =
            [ width fill
            , Theme.roundedBorder
            , Font.color fontColor
            , Background.color backgroundColor
            , Border.color borderColor
            , Border.width 1
            , onClick (options.toParentMsg <| Msg <| rowClickMsg item)
            , onMouseEnter (options.toParentMsg <| Msg <| RowHovered item)
            , onMouseLeave (options.toParentMsg <| Msg <| ClearHoverRow)
            , onDoubleClick (options.toParentMsg <| Msg <| RowDoubleClicked item)
            , paddingXY 10 5
            , pointer
            ]
    in
    row rowAttrs <|
        case options.width of
            SameWidth ->
                List.map (cellView Nothing options.textAlignment item) options.body

            CustomWidth portions ->
                List.map2 (\portion fn -> cellView (Just portion) options.textAlignment item fn) portions options.body


cellView : Maybe Int -> TextAlignment -> item -> (item -> Element msg) -> Element msg
cellView maybePortion alignment item viewFn =
    el
        [ widthFromMaybeInt maybePortion
        , textAlignmentAttr alignment
        ]
        (viewFn item)


headerView : Options msg item -> Element msg
headerView options =
    if List.length options.header == 0 then
        none

    else
        let
            headerAttr =
                [ Font.bold
                , textAlignmentAttr options.textAlignment
                , Font.color Theme.gray2
                , width fill

                -- Hack to comply with the body row border
                , paddingXY 11 10
                ]
        in
        row headerAttr <|
            case options.width of
                SameWidth ->
                    List.map (headerItemView Nothing) options.header

                CustomWidth portions ->
                    List.map2
                        (\headerEl portion -> headerItemView (Just portion) headerEl)
                        options.header
                        portions


headerItemView : Maybe Int -> Element msg -> Element msg
headerItemView maybeWidth headerEl =
    el [ widthFromMaybeInt maybeWidth ] headerEl


widthFromMaybeInt : Maybe Int -> Attribute msg
widthFromMaybeInt maybePortion =
    width <|
        case maybePortion of
            Just portion ->
                fillPortion portion

            Nothing ->
                fill



-- Helpers


textAlignmentAttr : TextAlignment -> Attribute msg
textAlignmentAttr alignment =
    case alignment of
        Left ->
            Font.alignLeft

        Center ->
            Font.center

        Right ->
            Font.alignRight


isItemHovered : item -> State item comparable -> Bool
isItemHovered item (State state) =
    state.tableRowHovered
        |> Maybe.map (\selectedItem -> state.toComparable selectedItem == state.toComparable item)
        |> Maybe.withDefault False


isItemSelected : item -> State item comparable -> Bool
isItemSelected item (State state) =
    state.tableRowSelected
        |> Maybe.map (\selectedItem -> state.toComparable selectedItem == state.toComparable item)
        |> Maybe.withDefault False


sendMsgToOutside : msg -> Cmd msg
sendMsgToOutside msg =
    Task.succeed msg |> Task.perform identity

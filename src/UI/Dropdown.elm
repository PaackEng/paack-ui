module UI.Dropdown exposing
    ( Dropdown, BasicConfig, basic, filterable
    , State, Msg, init, update
    , withPlaceholder, withFilterPlaceholder
    , withSize
    , withItems, withSelected
    , withItemToText, withItemToPrompt
    , withMaximumListHeight, withListWidth, withListAlignedRight
    , renderElement
    )

{-| Accessible and uniform-styled implementation of a dropdown menu.

    Dropdown.basic
        { dropdownMsg = ForDropdownMsg
        , onSelectMsg = GotSelectItemMsg
        , state = model.dropdownState
        }
        |> Dropdown.withPlaceholder "Choose a book"
        |> Dropdown.withItems model.books
        |> Dropdown.withSelected model.selectedBook
        |> Dropdown.withItemToText Books.getTitle
        |> Dropdown.renderElement renderConfig


# Dropdown

@docs Dropdown, BasicConfig, basic, filterable


## State

@docs State, Msg, init, update


# Options

@docs withPlaceholder, withFilterPlaceholder
@docs withSize
@docs withItems, withSelected
@docs withItemToText, withItemToPrompt
@docs withMaximumListHeight, withListWidth, withListAlignedRight


# Rendering

@docs renderElement

-}

import Dropdown
import Element exposing (Attribute, Element, maximum, shrink)
import Element.Background as Background
import Element.Border as Border
import UI.Effects as Effects exposing (Effects)
import UI.Icon as Icon
import UI.Internal.Colors as Colors
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size exposing (Size)
import UI.Text as Text


{-| The `Dropdown item msg` type is used for describing the component for later rendering.
-}
type Dropdown item msg
    = Dropdown (Properties item msg) (Options item)


type alias Properties item msg =
    { dropdownType : DropdownType
    , items : List item
    , dropdownMsg : Msg item -> msg
    , onSelectMsg : Maybe item -> msg
    , state : State item
    }


type alias Options item =
    { placeholder : Maybe String
    , filterPlaceholder : Maybe String
    , selected : Maybe item
    , itemToText : item -> String
    , itemToPrompt : Maybe (item -> String)
    , listHeight : Maybe Int
    , listWidth : Maybe Int
    , listRightAlign : Bool
    , size : Size
    }


{-| Keep this one in your Model, it holds the dropdown's current state.
-}
type State item
    = State (InternalState item)


type alias InternalState item =
    Dropdown.State item


{-| Opaque type for the internal dropdown messages
-}
type Msg item
    = Msg (Dropdown.Msg item)


type DropdownType
    = Basic
    | Filterable


{-| `BasicConfig` assembles the required configuration for having a simple dropdown.

    { dropdownMsg = ForDropdownMsg
    , onSelectMsg = GotSelectItemMsg
    , state = model.dropdownState
    }

-}
type alias BasicConfig item msg =
    { dropdownMsg : Msg item -> msg
    , onSelectMsg : Maybe item -> msg
    , state : State item
    }


{-| The correct way of instantiating a [`Dropdown.State`](#State).

    { -- ...
    , state = Dropdown.init "dropdown-id"
    -- ...
    }

-}
init : String -> State item
init id =
    State <| Dropdown.init id


{-| Given a message, apply an update to the [`Dropdown.State`](#State).
Do not ignore the returned `Effect`, it may include remote select's messages.

    ( newModel, newCmd ) =
        Dropdown.update renderConfig subMsg dropdown

-}
update : RenderConfig -> Msg item -> Dropdown item msg -> ( State item, Effects msg )
update cfg (Msg msg) dropdown =
    let
        (Dropdown prop _) =
            dropdown

        (State state) =
            prop.state

        ( state_, effects ) =
            Dropdown.updateWithoutPerform (getConfig cfg dropdown) msg () state
    in
    ( State state_, List.map translateEffect effects )


translateEffect : Dropdown.Effect msg -> Effects.SideEffect msg
translateEffect effect =
    case effect of
        Dropdown.Loopback msg ->
            Effects.MsgToCmd msg

        Dropdown.DomFocus msg id ->
            Effects.DomFocus msg id


{-| Constructs a basic dropdown.
Also defines the handling function for messages, and the current dropdown's state.

    Dropdown.basic
        { dropdownMsg = ForDropdownMsg
        , onSelectMsg = GotSelectItemMsg
        , state = model.dropdownState
        }

-}
basic : BasicConfig item msg -> Dropdown item msg
basic prop =
    Dropdown
        { dropdownType = Basic
        , items = []
        , dropdownMsg = prop.dropdownMsg
        , onSelectMsg = prop.onSelectMsg
        , state = prop.state
        }
        defaultOptions


{-| Constructs a filterable dropdown.
Also defines the handling function for messages, and the current dropdown's state.

    Dropdown.filterable
        { dropdownMsg = ForDropdownMsg
        , onSelectMsg = GotSelectItemMsg
        , state = model.dropdownState
        }

-}
filterable : BasicConfig item msg -> Dropdown item msg
filterable prop =
    Dropdown
        { dropdownType = Filterable
        , items = []
        , dropdownMsg = prop.dropdownMsg
        , onSelectMsg = prop.onSelectMsg
        , state = prop.state
        }
        defaultOptions


defaultOptions : Options item
defaultOptions =
    { placeholder = Nothing
    , filterPlaceholder = Nothing
    , selected = Nothing
    , itemToText = always ""
    , itemToPrompt = Nothing
    , listHeight = Nothing
    , listWidth = Nothing
    , listRightAlign = False
    , size = Size.extraSmall
    }


{-| Changes the component's placeholder text.

    Dropdown.withPlaceholder "Choose a book"
        someDropdown

-}
withPlaceholder : String -> Dropdown item msg -> Dropdown item msg
withPlaceholder placeholder (Dropdown prop opt) =
    Dropdown prop { opt | placeholder = Just placeholder }


{-| Changes the filterable component's placeholder text.

    Dropdown.withPlaceholder "Choose a book"
        someDropdown

-}
withFilterPlaceholder : String -> Dropdown item msg -> Dropdown item msg
withFilterPlaceholder placeholder (Dropdown prop opt) =
    Dropdown prop { opt | filterPlaceholder = Just placeholder }


{-| With `Dropdown.withSize`, you'll be able to scale the field between the [standard sizes][size].

[size]: UI-Size

The sizes (in height) are: Large - 60px; Medium - 48px; Small - 36px; Extra Small - 28px.

    Dropdown.withSize Size.large someField

**NOTE**: Dropdown's default size is [`Size.medium`](UI-Size#medium)

-}
withSize : Size -> Dropdown item msg -> Dropdown item msg
withSize size (Dropdown prop opt) =
    Dropdown prop
        { opt | size = size }


{-| Changes the component's list of elements.

    Dropdown.withItems
        [ { id = 0, name = "Entry A" }
        , { id = 1, name = "Entry B" }
        ]
        someDropdown

-}
withItems : List item -> Dropdown item msg -> Dropdown item msg
withItems items (Dropdown prop opt) =
    Dropdown { prop | items = items } opt


{-| Marks the element as selected.

    Dropdown.withSelected (List.head model.entries)
        someDropdown

-}
withSelected : Maybe item -> Dropdown item msg -> Dropdown item msg
withSelected selected (Dropdown prop opt) =
    Dropdown prop { opt | selected = selected }


{-| Changes the way elements are formatted.

    Dropdown.withItemToText .name
        someDropdown

-}
withItemToText : (item -> String) -> Dropdown item msg -> Dropdown item msg
withItemToText itemToText (Dropdown prop opt) =
    Dropdown prop { opt | itemToText = itemToText }


{-| Changes the way prompt is formatted.

    Dropdown.withItemToPrompt (.name >> String.toUpper)
        someDropdown

-}
withItemToPrompt : (item -> String) -> Dropdown item msg -> Dropdown item msg
withItemToPrompt toPrompt (Dropdown prop opt) =
    Dropdown prop { opt | itemToPrompt = Just toPrompt }


{-| Changes the maximum height of the dropdown list.

    Dropdown.withMaximumListHeight 200
        someDropdown

-}
withMaximumListHeight : Int -> Dropdown item msg -> Dropdown item msg
withMaximumListHeight height (Dropdown prop opt) =
    Dropdown prop { opt | listHeight = Just height }


{-| Set the width of list dropdown, in case it needs to have a different width
then the prompt.

    Dropdown.withListWidth 200
        someDropdown

-}
withListWidth : Int -> Dropdown item msg -> Dropdown item msg
withListWidth width (Dropdown prop opt) =
    Dropdown prop { opt | listWidth = Just width }


{-| Sets the alignment of dropdown list in case its width differs from prompt.

    Dropdown.withListAlignedRight True
        someDropdown

-}
withListAlignedRight : Bool -> Dropdown item msg -> Dropdown item msg
withListAlignedRight shouldAlignRight (Dropdown prop opt) =
    Dropdown prop { opt | listRightAlign = shouldAlignRight }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Dropdown item msg -> Element msg
renderElement cfg dropdown =
    let
        (Dropdown prop _) =
            dropdown

        (State state) =
            prop.state
    in
    Dropdown.view (getConfig cfg dropdown) () state



-- Internals


getConfig : RenderConfig -> Dropdown item msg -> Dropdown.Config item msg ()
getConfig cfg ((Dropdown prop opt) as dropdown) =
    case prop.dropdownType of
        Basic ->
            Dropdown.basic
                { itemsFromModel = always prop.items
                , selectionFromModel = always opt.selected
                , dropdownMsg = Msg >> prop.dropdownMsg
                , onSelectMsg = prop.onSelectMsg
                , itemToPrompt = itemToPrompt cfg dropdown
                , itemToElement = itemToElement cfg dropdown
                }
                |> customDropdown cfg dropdown

        Filterable ->
            Dropdown.filterable
                { itemsFromModel = always prop.items
                , selectionFromModel = always opt.selected
                , dropdownMsg = Msg >> prop.dropdownMsg
                , onSelectMsg = prop.onSelectMsg
                , itemToPrompt = itemToPrompt cfg dropdown
                , itemToElement = itemToElement cfg dropdown
                , itemToText = opt.itemToText
                }
                |> Dropdown.withFilterPlaceholder
                    (Maybe.withDefault (Maybe.withDefault "" opt.placeholder)
                        opt.filterPlaceholder
                    )
                |> customDropdown cfg dropdown


itemToPrompt : RenderConfig -> Dropdown item msg -> item -> Element msg
itemToPrompt cfg (Dropdown _ opts) item =
    Text.subtitle2
        (case opts.itemToPrompt of
            Just toPrompt ->
                toPrompt item

            Nothing ->
                opts.itemToText item
        )
        |> Text.withColor Palette.blue700
        |> Text.renderElement cfg


itemToElement : RenderConfig -> Dropdown item msg -> Bool -> Bool -> item -> Element msg
itemToElement cfg (Dropdown _ opts) selected highlighted item =
    Element.el
        [ Background.color <|
            if selected then
                Palette.toElementColor Palette.blue700

            else if highlighted then
                Palette.toElementColor Palette.gray300

            else
                Colors.white
        , Element.paddingXY 12 8
        , Element.width Element.fill
        , Border.rounded 3
        ]
        (Text.body2 (opts.itemToText item)
            |> Text.withColor
                (if selected then
                    Palette.gray100

                 else
                    Palette.gray700
                )
            |> Text.renderElement cfg
        )


customDropdown : RenderConfig -> Dropdown item msg -> Dropdown.Config item msg () -> Dropdown.Config item msg ()
customDropdown cfg dropdown =
    Dropdown.withContainerAttributes [ Element.width Element.fill ]
        >> Dropdown.withPromptElement (promptElement cfg dropdown)
        >> Dropdown.withSelectAttributes (selectAttrs dropdown)
        >> Dropdown.withListAttributes (listAttrs dropdown)
        >> Dropdown.withSearchAttributes [ Border.width 0, Element.padding 0 ]
        >> Dropdown.withOpenCloseButtons (openCloseButtons cfg dropdown)


promptElement : RenderConfig -> Dropdown item msg -> Element msg
promptElement cfg (Dropdown _ opt) =
    Text.body2 (Maybe.withDefault "" <| opt.placeholder)
        |> Text.withColor Palette.gray800
        |> Text.renderElement cfg


selectAttrs : Dropdown item msg -> List (Attribute msg)
selectAttrs (Dropdown prop _) =
    Element.width Element.fill
        :: Element.paddingXY 14 10
        :: Border.width 1
        :: Border.color (Palette.toElementColor Palette.gray300)
        :: Border.rounded 6
        :: Background.color (Palette.toElementColor Palette.gray100)
        :: (case prop.dropdownType of
                Basic ->
                    [ Element.pointer ]

                Filterable ->
                    []
           )


listAttrs : Dropdown item msg -> List (Attribute msg)
listAttrs (Dropdown _ opt) =
    [ case opt.listWidth of
        Just width ->
            Element.width <| Element.px width

        Nothing ->
            Element.width Element.fill
    , if opt.listRightAlign then
        Element.alignRight

      else
        Element.alignLeft
    , case opt.listHeight of
        Just height ->
            Element.height <| maximum height shrink

        Nothing ->
            Element.height Element.fill
    , Element.scrollbarY
    , Element.padding 4
    , Border.width 1
    , Border.color <| Palette.toElementColor Palette.gray200
    , Border.rounded 4
    , Border.shadow
        { offset = ( 0, 2 )
        , size = 0
        , blur = 80
        , color = shadowColor
        }
    , Border.shadow
        { offset = ( 0, 4 )
        , size = 0
        , blur = 16
        , color = shadowColor
        }
    , Colors.mainBackground
    , Element.pointer
    ]


openCloseButtons : RenderConfig -> Dropdown item msg -> { openButton : Element msg, closeButton : Element msg }
openCloseButtons cfg (Dropdown _ opts) =
    let
        renderButton =
            Icon.withColor Palette.blue700
                >> Icon.withSize opts.size
                >> Icon.renderElement cfg
                >> Element.el [ Element.alignTop, Element.alignRight ]

        dropdownLocaleTerms =
            cfg |> localeTerms >> .dropdown
    in
    { openButton =
        renderButton <| Icon.toggleDown dropdownLocaleTerms.show
    , closeButton =
        renderButton <| Icon.close dropdownLocaleTerms.collapse
    }


shadowColor : Element.Color
shadowColor =
    Element.rgba 0 0 0 0.04

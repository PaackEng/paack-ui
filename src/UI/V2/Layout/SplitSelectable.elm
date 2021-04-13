module UI.V2.Layout.SplitSelectable exposing
    ( Config, MobileConfig
    , mobile, desktop
    )

{-| The `UI.V2.Layout.SplitSelectable` provide helpers to render a view for
displaying a list of items where selecting any particular item opens a detailed
view of it. You will have to generate a
[`Nav.Content`](UI-NavigationContainer#Content) through [`mobile`](#mobile) or
[`desktop`](#desktop) helper depending upon the target device, which later a
page can provide through [`Nav.Container`](UI-NavigationContainer#Container) to
show on screens.

Example of usage:

    splitSelectableContent : RenderConfig -> Config -> Nav.Content
    splitSelectableContent renderConfig layoutConfig =
        desktop renderConfig layoutConfig

later on, `splitSelectableContent` can be used to populate the `content` field
inside the [`Container`](UI-NavigationContainer#Container) record to render it.


# Types

@docs Config, MobileConfig


# Building

@docs mobile, desktop

-}

import Element exposing (Element, fill, fillPortion, minimum)
import Element.Border as Border
import Element.Keyed as Keyed
import UI.ListView as ListView exposing (ListView)
import UI.NavigationContainer as Nav
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Utils.Action as Action
import UI.Utils.Element as Element exposing (zeroPadding)



-- Types


{-| The `Config object msg` is used to construct a record which later can
be fed to [`mobile`](#mobile) or [`desktop`](#desktop) helper to construct a
SplitSelectable view.
-}
type alias Config object msg =
    { getKey : object -> String
    , items : List object
    , listView : ListView object msg
    , selected : Maybe String
    , selectedView : Element msg
    , withAboveElement : Maybe (Element msg)
    }


{-| The `MobileConfig msg` is used to construct a record having mobile specific
parts of the data items needed to build a SplitSelectable view.
-}
type alias MobileConfig msg =
    { action : Maybe (Action.WithIcon msg)
    , title : ( String, Maybe String )
    , unselectMsg : msg
    }



-- Building


{-| `mobile` helper constructs SplitSelectable layout for rendering on mobile
-}
mobile : RenderConfig -> MobileConfig msg -> Config object msg -> Nav.Content msg
mobile renderConfig mobileConfig layoutConfig =
    case layoutConfig.selected of
        Just _ ->
            Nav.contentStackChild
                { title = mobileConfig.title
                , goBackMsg = mobileConfig.unselectMsg
                , action = mobileConfig.action
                }
                layoutConfig.selectedView

        Nothing ->
            layoutConfig
                |> listContent renderConfig
                |> Nav.contentSingle


{-| `desktop` helper constructs SplitSelectable layout for rendering on desktop
-}
desktop : RenderConfig -> Config object msg -> Nav.Content msg
desktop renderConfig layoutConfig =
    Keyed.row
        [ Element.width fill
        , Element.height fill
        , Element.maxHeightPct 100
        , Element.clipY
        ]
        [ ( "list", listColumn renderConfig layoutConfig )
        , ( "selected", selectedColumn layoutConfig )
        ]
        |> withAboveElement layoutConfig.withAboveElement
        |> Nav.contentSingle



-- Internals


listColumn : RenderConfig -> Config object msg -> Element msg
listColumn renderConfig layoutConfig =
    Element.el
        [ Element.width (listWidth renderConfig)
        , Element.height fill
        , Border.widthEach { zeroPadding | right = 1 }
        , Palette.brightnessLightest
            |> Palette.color Palette.toneGray
            |> Palette.toElementColor
            |> Border.color
        ]
        (listContent renderConfig layoutConfig)


listContent : RenderConfig -> Config object msg -> Element msg
listContent renderConfig { items, listView, selected, getKey } =
    listView
        |> ListView.withItems items
        |> ListView.withSelected
            (getKey >> Just >> (==) selected)
        |> ListView.withWidth fill
        |> ListView.renderElement renderConfig


listWidth : RenderConfig -> Element.Length
listWidth renderConfig =
    if RenderConfig.isMobile renderConfig then
        fill

    else
        fillPortion 25 |> minimum 228


selectedColumn : Config object msg -> Element msg
selectedColumn layoutConfig =
    Keyed.el
        [ Element.width (fillPortion 75)
        , Element.height fill
        , Element.scrollbarY
        ]
        ( selectedKey layoutConfig
        , layoutConfig.selectedView
        )


selectedKey : Config object msg -> String
selectedKey { selected } =
    case selected of
        Just key ->
            "#" ++ key

        Nothing ->
            "@none"

withAboveElement : Maybe (Element msg) -> Element msg -> Element msg
withAboveElement maybeHeader body =
    case maybeHeader of
        Just header ->
            Element.column [Element.width fill, Element.height fill]
                [ header, body ]

        Nothing ->
            body

module UI.Layout.SplitSelectable exposing (Config, MobileConfig, desktop, mobile)

import Element exposing (Element, fill, fillPortion, minimum)
import Element.Border as Border
import Element.Keyed as Keyed
import UI.Button exposing (Button)
import UI.ListView as ListView exposing (ListView)
import UI.NavigationContainer as Nav
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Utils.Element as Element exposing (zeroPadding)


type alias Config object msg =
    { getKey : object -> String
    , items : List object
    , listView : ListView object msg
    , selected : Maybe String
    , selectedView : Element msg
    }


type alias MobileConfig msg =
    { rightButton : Maybe (Button msg)
    , title : ( String, Maybe String )
    , unselectMsg : msg
    }


mobile : RenderConfig -> MobileConfig msg -> Config object msg -> Nav.Content msg
mobile renderConfig mobileConfig layoutConfig =
    case layoutConfig.selected of
        Just _ ->
            Nav.contentStackChild
                { title = mobileConfig.title
                , goBackMsg = mobileConfig.unselectMsg
                , rightButton = mobileConfig.rightButton
                }
                layoutConfig.selectedView

        Nothing ->
            layoutConfig
                |> listContent renderConfig
                |> Nav.contentSingle


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
        |> Nav.contentSingle


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

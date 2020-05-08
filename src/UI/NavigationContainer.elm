module UI.NavigationContainer exposing
    ( Container
    , Content
    , MenuAction
    , MenuPage
    , Msg
    , Navigator
    , State
    , containerMap
    , contentSingle
    , menuAction
    , menuPage
    , navigator
    , stateInit
    , stateUpdate
    , toEl
    , withMenuActions
    , withMenuLogo
    , withMenuPages
    )

import Element exposing (Attribute, Element)
import Html exposing (Html)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Menu as Menu
import UI.Internal.SideBar as SideBar
import UI.Link as Link exposing (Link)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)



-- Expose


type alias MenuPage =
    Menu.Page


type alias MenuAction msg =
    Menu.Action msg


type alias Menu msg =
    Menu.Menu msg



-- Types


type alias State =
    -- Keep this one in MODEL
    { menuExpanded : Bool }


type Msg
    = ToggleMenu Bool


type Content msg
    = ContentSingle (RenderConfig -> Element msg)


type alias Dialog msg =
    { title : String
    , close : msg
    , body : RenderConfig -> Element msg
    }


type alias Container msg =
    { content : Content msg
    , title : String
    , dialog : Maybe (Dialog msg)
    , hasMenu : Bool
    }


type alias Navigator page msg =
    { container : page -> Container msg
    , menu : Menu msg
    }



-- Options


withMenuPages : List MenuPage -> Navigator page msg -> Navigator page msg
withMenuPages pages nav =
    let
        menuWithPages (Menu.Menu prop opt) =
            Menu.Menu prop { opt | pages = pages }
    in
    { nav | menu = menuWithPages nav.menu }


withMenuActions : List (MenuAction msg) -> Navigator page msg -> Navigator page msg
withMenuActions actions nav =
    let
        menuWithActions (Menu.Menu prop opt) =
            Menu.Menu prop { opt | actions = actions }
    in
    { nav | menu = menuWithActions nav.menu }


withMenuLogo : String -> Element msg -> Navigator page msg -> Navigator page msg
withMenuLogo hint body nav =
    let
        menuWithLogo (Menu.Menu prop opt) =
            Menu.Menu prop
                { opt | logo = Just <| Menu.Logo hint body }
    in
    { nav | menu = menuWithLogo nav.menu }



-- Helpers


containerMap : (a -> b) -> Container a -> Container b
containerMap applier data =
    { title = data.title
    , hasMenu = data.hasMenu
    , content = contentMap applier data.content
    , dialog = Maybe.map (dialogMap applier) data.dialog
    }


contentMap : (a -> b) -> Content a -> Content b
contentMap applier data =
    case data of
        ContentSingle element ->
            element
                |> lazyMap (Element.map applier)
                |> ContentSingle


dialogMap : (a -> b) -> Dialog a -> Dialog b
dialogMap applier data =
    { title = data.title
    , close = applier data.close
    , body = lazyMap (Element.map applier) data.body
    }


stateUpdate : Msg -> State -> ( State, Cmd Msg )
stateUpdate msg state =
    case msg of
        ToggleMenu newVal ->
            ( { state | menuExpanded = newVal }, Cmd.none )



-- Constructors


stateInit : State
stateInit =
    { menuExpanded = True }


contentSingle : (RenderConfig -> Element msg) -> Content msg
contentSingle body =
    ContentSingle body


menuPage : Icon -> Link -> Bool -> MenuPage
menuPage icon link isCurrent =
    Menu.Page icon link isCurrent


menuAction : Icon -> msg -> MenuAction msg
menuAction icon msg =
    Menu.Action icon msg


navigator :
    (Msg -> msg)
    -> State
    -> (page -> Container msg)
    -> Navigator page msg
navigator applier state pagesContainers =
    Navigator pagesContainers
        (menu applier state)



-- Render


toEl :
    RenderConfig
    -> page
    -> Navigator page msg
    -> { body : List (Html msg), title : String }
toEl cfg page model =
    let
        { content, title, dialog, hasMenu } =
            model.container page

        contentBody =
            case content of
                ContentSingle single ->
                    single cfg

        body =
            -- HERE WILL GO THE RESPONSIVE MAGIC (Drawer vs SideBar vs TitleBar)
            if hasMenu then
                SideBar.desktopColumn cfg contentBody model.menu

            else
                contentBody

        defaultAttrs =
            RenderConfig.elLayoutAttributes cfg
    in
    { title = title
    , body = [ Element.layout defaultAttrs body ]
    }



-- Internals


menu : (Msg -> msg) -> State -> Menu msg
menu applier { menuExpanded } =
    Menu.default (ToggleMenu >> applier) menuExpanded


lazyMap : (a -> b) -> (c -> a) -> (c -> b)
lazyMap applier original =
    \whatever -> applier (original whatever)

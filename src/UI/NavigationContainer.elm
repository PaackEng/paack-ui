module UI.NavigationContainer exposing
    ( Container
    , Content
    , Dialog
    , MenuAction
    , MenuPage
    , Msg
    , Navigator
    , StackChild
    , State
    , containerMap
    , contentSingle
    , contentStackChild
    , dialog
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
import Element.Background as Background
import Element.Events as Events
import Html exposing (Html)
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Basics exposing (lazyMap)
import UI.Internal.Dialog as Dialog exposing (dialogMap)
import UI.Internal.Menu as Menu
import UI.Internal.Palette as Palette
import UI.Internal.SideBar as SideBar
import UI.Link as Link exposing (Link)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element as Element



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
    = ContentSingle (Element msg)
    | ContentStackChild (StackChild msg) (Element msg)


type alias Dialog msg =
    Dialog.Dialog msg


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


type alias StackChild msg =
    { title : String
    , buttons : List (Button msg)
    , goBackMsg : msg
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
                |> Element.map applier
                |> ContentSingle

        ContentStackChild stack element ->
            element
                |> Element.map applier
                |> ContentStackChild
                    { title = stack.title
                    , goBackMsg = applier stack.goBackMsg
                    , buttons = List.map (Button.map applier) stack.buttons
                    }


stateUpdate : Msg -> State -> ( State, Cmd Msg )
stateUpdate msg state =
    case msg of
        ToggleMenu newVal ->
            ( { state | menuExpanded = newVal }, Cmd.none )



-- Constructors


stateInit : RenderConfig -> State
stateInit cfg =
    { menuExpanded = not <| RenderConfig.isMobile cfg }


contentSingle : Element msg -> Content msg
contentSingle body =
    ContentSingle body


contentStackChild : StackChild msg -> Element msg -> Content msg
contentStackChild prop body =
    ContentStackChild prop body


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


dialog : String -> msg -> Element.Length -> Element msg -> Dialog msg
dialog title onClose width body =
    { title = title
    , close = onClose
    , width = width
    , body = body
    }



-- Render


toEl :
    RenderConfig
    -> page
    -> Navigator page msg
    -> { body : List (Html msg), title : String }
toEl cfg page model =
    let
        container =
            model.container page

        { content, title, hasMenu } =
            container

        ( contentBody, maybeGoBack, seenTitle ) =
            contentProps title content

        body =
            if hasMenu then
                if RenderConfig.isMobile cfg then
                    SideBar.mobileDrawer cfg
                        contentBody
                        model.menu
                        seenTitle
                        maybeGoBack

                else
                    SideBar.desktopColumn cfg contentBody model.menu

            else
                contentBody

        bodyWithModal =
            case container.dialog of
                Just state ->
                    Element.el
                        [ Element.inFront (Dialog.view cfg state)
                        , Element.width Element.fill
                        , Element.height Element.fill
                        ]
                        body

                Nothing ->
                    body

        defaultAttrs =
            RenderConfig.elLayoutAttributes cfg
    in
    { title = title
    , body = [ Element.layout defaultAttrs bodyWithModal ]
    }



-- Internals


menu : (Msg -> msg) -> State -> Menu msg
menu applier { menuExpanded } =
    Menu.default (ToggleMenu >> applier) menuExpanded


contentProps : String -> Content msg -> ( Element msg, Maybe msg, String )
contentProps mainTitle content =
    case content of
        ContentSingle body ->
            ( body, Nothing, mainTitle )

        ContentStackChild { title, goBackMsg } body ->
            ( body, Just goBackMsg, title )

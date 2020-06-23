module UI.NavigationContainer exposing
    ( Msg, State, stateInit, stateUpdate
    , Navigator, navigator
    , Container, containerMap
    , Content, contentSingle, StackChild, contentStackChild
    , withMenuLogo, withMenuActions, MenuAction, menuAction, withMenuPages, MenuPage, menuPage
    , Dialog, dialog
    , renderElement
    )

{-| The `UI.NavigationContainer` (abbreviated as `Nav`) is a page presenter.
Depending on the situation, it applies the sidebar menu, dialogs, and mobile's navbar over the current page.

For this, a page must provide some required data through the [`Nav.Container`](UI-NavigationContainer#Container) record.
That aggregated with the current rendering configuration provided by [`UI.RenderConfig`](UI-RenderConfig#RenderConfig) and the [`Nav.State`](UI-NavigationContainer#State), multiplex between possible viewing layouts.

Example of usage:

    view : RenderConfig -> Model.Model -> { body : List (Html Msg.Msg), title : String }
    view renderConfig { navState, currentPage } =
        Nav.navigator Msg.NavMsg
            navState
            (getPageContainer >> Nav.containerMap Msg.PageMsg)
            |> Nav.withMenuPages
                [ Nav.menuPage (Icon.packages "Packages")
                    (Link.link "/packages")
                    (currentPage == Page.Packages)
                ]
            |> Nav.withMenuActions
                [ Nav.menuAction
                    (Icon.logout "Logout")
                    Msg.SessionLogout
                ]
            |> Nav.withMenuLogo "My company's logo" someLogoElement
            |> Nav.renderElement renderConfig currentPage

    getPageContainer : Page.Page -> Nav.Container Page.Msg
    getPageContainer page =
        case page of
            Page.Packages ->
                { title = "Pacakges"
                , content = Nav.contentSingle Packages.view
                , dialog = Nothing
                , hasMenu = False
                }


# Model & Update

@docs Msg, State, stateInit, stateUpdate


# Building

@docs Navigator, navigator


# Page

@docs Container, containerMap


# Content

@docs Content, contentSingle, StackChild, contentStackChild


# Menu

@docs withMenuLogo, withMenuActions, MenuAction, menuAction, withMenuPages, MenuPage, menuPage


# Dialog

@docs Dialog, dialog


# Rendering

@docs renderElement

-}

import Element exposing (Element)
import Html exposing (Html)
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Dialog as Dialog exposing (dialogMap)
import UI.Internal.Menu as Menu
import UI.Internal.SideBar as SideBar
import UI.Link exposing (Link)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Utils.Element as Element



-- Expose


{-| This record must be generated with [`Nav.menuPage`](UI-NavigationContainer#menuPage)
-}
type alias MenuPage =
    Menu.Page


{-| This record must be generated with [`Nav.menuAction`](UI-NavigationContainer#menuAction)
-}
type alias MenuAction msg =
    Menu.Action msg



-- Types


{-| Keep this one in your Model, it holds the current navigation state.
-}
type alias State =
    { menuExpanded : Bool }


{-| The `Nav.Msg` handles menu related messages transparently.
-}
type Msg
    = ToggleMenu Bool


{-| The `Nav.Content msg` manages different kinds of pages' body.
By now, the content is either a typical single page or a stacked child of mobile's views.

The typical single page renders the way they come.
The stacked child has a different header on mobile, where a back button replaces the sandwich button.

-}
type Content msg
    = ContentSingle (Element msg)
    | ContentStackChild (StackChild msg) (Element msg)


{-| The `Nav.Dialog msg` is a record holding the description of a dialog.
See [`Nav.dialog`](UI-NavigationContainer#dialog) to see how to create a dialog.
-}
type alias Dialog msg =
    Dialog.Dialog msg


{-| The `Nav.Container msg` describes the current page in its current state.

The `title` field is exposed as to the browser and reused on the mobile's navbar.

When it is `Just someElement`, the `dialog` field is shown over the page.

The `hasMenu` field can hide the menu when undesired, e.g., login page.

The `content` field must be the element holding the page's view.

-}
type alias Container msg =
    { content : Content msg
    , title : String
    , dialog : Maybe (Dialog msg)
    , hasMenu : Bool
    }


type alias Navigator page msg =
    { container : page -> Container msg
    , menu : Menu.Menu msg
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


dialog : String -> msg -> Element msg -> Dialog msg
dialog title onClose body =
    { title = title
    , close = onClose
    , body = body
    }



-- Render


renderElement :
    RenderConfig
    -> page
    -> Navigator page msg
    -> { body : List (Html msg), title : String }
renderElement cfg page model =
    let
        container =
            model.container page

        { content, title, hasMenu } =
            container

        ( contentBody, maybeStack, seenTitle ) =
            contentProps title content

        body =
            if hasMenu then
                if RenderConfig.isMobile cfg then
                    SideBar.mobileDrawer cfg
                        contentBody
                        model.menu
                        seenTitle
                        maybeStack

                else
                    SideBar.desktopColumn cfg contentBody model.menu

            else
                contentBody

        dialogView =
            case container.dialog of
                Just state ->
                    Dialog.view cfg state

                Nothing ->
                    Element.none

        bodyWithDialog =
            -- Always creating this element is required so we don't loose scrollbar state
            Element.el
                [ Element.inFront dialogView
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                body

        defaultAttrs =
            RenderConfig.elLayoutAttributes cfg
    in
    { title = title
    , body =
        [ Icon.svgSpriteImport
        , Element.layout defaultAttrs bodyWithDialog
        ]
    }



-- Internals


menu : (Msg -> msg) -> State -> Menu.Menu msg
menu applier { menuExpanded } =
    Menu.default (ToggleMenu >> applier) menuExpanded


contentProps : String -> Content msg -> ( Element msg, Maybe ( msg, List (Button msg) ), String )
contentProps mainTitle content =
    case content of
        ContentSingle body ->
            ( body, Nothing, mainTitle )

        ContentStackChild { title, goBackMsg, buttons } body ->
            ( body, Just ( goBackMsg, buttons ), title )

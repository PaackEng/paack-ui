module UI.Page exposing
    ( Msg, State, stateInit, stateWithClosedMenu, stateUpdate
    , Navigator, navigator
    , Page, page, pageWithDialog, pageWithDefaultMenu, pageMap
    , PageBody, bodySingle, Stack, bodyStack
    , withMenuLogo, withMenuActions, MenuAction, menuAction, withMenuPages
    , MenuPage, menuPage, withSidebarStyle, sidebarPersistent
    , sidebarNonPersistent, showMenu, hideMenu
    , toBrowserDocument
    )

{-| The `UI.Page` is a page presenter.
Depending on the situation, it applies the sidebar menu, dialogs, and mobile's navbar over the current page.

For this, a page must provide some required data through the [`Nav.Page`](#Page) record.
That aggregated with the current rendering configuration provided by [`UI.RenderConfig`](UI-RenderConfig#RenderConfig) and the [`Nav.State`](#State), multiplex between possible viewing layouts.

Example of usage:

    view : RenderConfig -> Model.Model -> { body : List (Html Msg.Msg), title : String }
    view renderConfig { navState, currentPage } =
        Nav.navigator Msg.NavMsg
            navState
            (getPagePage >> Nav.pageMap Msg.PageMsg)
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
            |> Nav.toBrowserDocument renderConfig currentPage

    getPagePage : Page.Page -> Nav.Page Page.Msg
    getPagePage page =
        case page of
            Page.Packages ->
                { title = "Packages"
                , content = Nav.bodySingle Packages.view
                , dialog = Nothing
                , hasMenu = False
                }


# Model & Update

@docs Msg, State, stateInit, stateWithClosedMenu, stateUpdate


# Building

@docs Navigator, navigator


# Page

@docs Page, page, pageWithDialog, pageWithDefaultMenu, pageMap


# PageBody

@docs PageBody, bodySingle, Stack, bodyStack


# Menu

@docs withMenuLogo, withMenuActions, MenuAction, menuAction, withMenuPages
@docs MenuPage, menuPage, withSidebarStyle, sidebarPersistent
@docs sidebarNonPersistent, showMenu, hideMenu


# Rendering

@docs toBrowserDocument

-}

import Element exposing (Element, fill)
import Html exposing (Html)
import UI.Effect as Effect exposing (Effect)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Menu as Menu
import UI.Internal.Page as Internal
import UI.Internal.SideBar as SideBar
import UI.Link exposing (Link)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Utils.Action as Action
import UI.V2.Dialog as Dialog2



-- Expose


{-| This record must be generated with [`Nav.menuPage`](#menuPage)
-}
type MenuPage
    = MenuPage Menu.Page


{-| This record must be generated with [`Nav.menuAction`](#menuAction)
-}
type MenuAction msg
    = MenuAction (Menu.Action msg)



-- Types


{-| Keep this one in your Model, it holds the current navigation state.
-}
type State
    = State StateRecord


type alias StateRecord =
    { menuExpanded : Bool }


{-| The `Nav.Msg` handles menu related messages.
-}
type Msg
    = ToggleMenu Bool


{-| The `Nav.PageBody msg` manages different kinds of pages' body.
By now, the content is either a typical single page or a stacked child of mobile's views.

The typical single page renders the way they come.
The stacked child has a different header on mobile, where a back button replaces the sandwich button.

-}
type alias PageBody msg =
    Internal.PageBody msg


{-| The `Nav.Page msg` describes the current page in its current state.

The `title` field is exposed as to the browser and reused on the mobile's navbar.

When it is `Just someElement`, the `dialog` field is shown over the page.

The `hasMenu` field can hide the menu when undesired, e.g., login page.

The `content` field must be the element holding the page's view.

    { content = Nav.bodySingle <| Element.el [] [ Element.text "Element body" ]
    , title = "Example page"
    , dialog = Nothing -- or Just <| Nav.dialog <| ...
    , hasMenu = True
    }

-}
type Page msg
    = Page (PageRecord msg)


type alias PageRecord msg =
    { content : PageBody msg
    , title : String
    , dialog : Maybe (Dialog msg)
    , hasMenu : Bool
    }


{-| The `Nav.Navigator` handles menu, dialogs and page viewing.
It must be initialized using [`Nav.navigator`](#navigator).
-}
type Navigator page msg
    = Navigator (NavigatorRecord page msg)


type alias NavigatorRecord page msg =
    { container : page -> Page msg
    , menu : Menu.Menu msg
    , sidebarStyle : SidebarStyle
    }


{-| The behavior/appearance that the sidebar will have when enabled
-}
type SidebarStyle
    = SidebarPersistent
    | SidebarNonPersistent


{-| Stacked children are typical on mobile.
The most significant difference is they have a customizable navbar where a back button replaces the sandwich menu, allowing the user to return to a higher scope.
Besides that, they can add custom buttons to the right side of the title.

This record holds a stack child's configuration.
That includes the back button's message, a title which overwrites the main page's title, and the customized buttons.

    { title = "Edit: Card " ++ selectedCard.number
    , buttons =
        [ Icon.print "Print card"
            |> Button.fromIcon
            |> Button.cmd (Msg.PrintCard selectedCard) Button.light
        ]
    , goBackMsg = Msg.DiscardCardChanges
    }

-}
type alias Stack msg =
    Internal.Stack msg



-- Options


{-| `Nav.withMenuPages` replaces the list of the menu's navigable pages.
Therefore changing the list of items showed at the top of the sidebar.

    Nav.withMenuPages
        [ Nav.menuPage (Icon.edit "Manage cards")
            (Link.link "/edit-cards")
            (currentPage == Page.CardsEdit)
        , Nav.menuPage (Icon.add "New manager")
            (Link.link "/add-manager")
            (currentPage == Page.ManagerAdd)
        ]
        someNav

-}
withMenuPages : List MenuPage -> Navigator page msg -> Navigator page msg
withMenuPages pages (Navigator nav) =
    let
        menuWithPages (Menu.Menu prop opt) =
            Menu.Menu prop { opt | pages = List.map (\(MenuPage p) -> p) pages }
    in
    Navigator { nav | menu = menuWithPages nav.menu }


{-| `Nav.withMenuActions` replaces the list of the menu's actions.
Therefore changing the list of items showed at the bottom of the sidebar.

    Nav.withMenuActions
        [ Nav.menuAction
            (Icon.language "Change to English")
            (Msg.SetLanguage Lang.English)
        , Nav.menuAction
            (Icon.logout "Logout")
            Msg.SessionLogout
        ]
        someNav

-}
withMenuActions : List (MenuAction msg) -> Navigator page msg -> Navigator page msg
withMenuActions actions (Navigator nav) =
    let
        menuWithActions (Menu.Menu prop opt) =
            Menu.Menu prop { opt | actions = List.map (\(MenuAction a) -> a) actions }
    in
    Navigator { nav | menu = menuWithActions nav.menu }


{-| `Nav.withMenuLogo` replaces the logo shown on the menu.
By now, this logo is only visible at the desktop's sidebar.

The first parameter is a hint that exists for accessibility reasons.

    Nav.withMenuLogo "Paack - Time Matters"
        Vectors.paackLogoWhite
        someNav

-}
withMenuLogo : String -> Element msg -> Navigator page msg -> Navigator page msg
withMenuLogo hint body (Navigator nav) =
    let
        menuWithLogo (Menu.Menu prop opt) =
            Menu.Menu prop
                { opt | logo = Just <| Menu.Logo hint body }
    in
    Navigator { nav | menu = menuWithLogo nav.menu }


{-| `Nav.withSidebarStyle` takes a `SidebarStyle` setting the
appearance/behavior of the sidebar when it is enabled by the `hasMenu` flag.
-}
withSidebarStyle : SidebarStyle -> Navigator page msg -> Navigator page msg
withSidebarStyle style (Navigator nav) =
    Navigator { nav | sidebarStyle = style }


{-| Persistent style of the sidebar. It occupies more space when open pushing
the content right.
-}
sidebarPersistent : SidebarStyle
sidebarPersistent =
    SidebarPersistent


{-| Non-persistent style of the sidebar. Like the mobile sidebar, this style
makes the sidebar open over the content with an overlay behind it.
-}
sidebarNonPersistent : SidebarStyle
sidebarNonPersistent =
    SidebarNonPersistent


{-| Message to force the exhibition of the sidebar/menu.
-}
showMenu : Msg
showMenu =
    ToggleMenu True


{-| Message to force hiding the sidebar/menu.
-}
hideMenu : Msg
hideMenu =
    ToggleMenu False



-- Helpers


{-| Transform the messages produced by a container.
-}
pageMap : (a -> b) -> Page a -> Page b
pageMap applier data =
    { title = data.title
    , hasMenu = data.hasMenu
    , content = contentMap applier data.content
    , dialog = Maybe.map (dialogMap applier) data.dialog
    }


contentMap : (a -> b) -> PageBody a -> PageBody b
contentMap applier data =
    case data of
        Internal.PageBodySingle element ->
            element
                |> Element.map applier
                |> Internal.PageBodySingle

        Internal.PageBodyStack stack element ->
            element
                |> Element.map applier
                |> Internal.PageBodyStack
                    { title = stack.title
                    , goBackMsg = applier stack.goBackMsg
                    , action = Maybe.map (Action.iconMap applier) stack.action
                    }


{-| Given a message, apply an update to the [`Nav.State`](#State).
-}
stateUpdate : Msg -> State -> ( State, Cmd Msg )
stateUpdate msg state =
    stateUpdateWithoutPerform msg state
        |> Tuple.mapSecond Effect.perform


{-| Similar to [`stateUpdate`], but using Effects.
-}
stateUpdateWithoutPerform : Msg -> State -> ( State, Effect Msg )
stateUpdateWithoutPerform msg (State state) =
    case msg of
        ToggleMenu newVal ->
            ( State { state | menuExpanded = newVal }, Effect.none )



-- Constructors


{-| The default way of instantiating a [`Nav.State`](#State).

    Nav.stateInit renderConfig

-}
stateInit : RenderConfig -> State
stateInit cfg =
    State { menuExpanded = not <| RenderConfig.isMobile cfg }


{-| Force the menu to be closed on the [`Nav.State`](#State).

    Nav.stateInitWithClosedMenu renderConfig

-}
stateWithClosedMenu : State -> State
stateWithClosedMenu (State state) =
    State { state | menuExpanded = False }


{-| `Nav.bodySingle` indicates that the current page is a simple single page.
It expects the final page's view in the only parameter.

    Nav.bodySingle <| view renderConfig model

-}
bodySingle : Element msg -> PageBody msg
bodySingle body =
    Internal.PageBodySingle body


{-| `Nav.bodyStack` indicates that the current page is a stack child's page.
It expects the child's configuration and the final page's view as its parameters.

    Nav.bodyStack
        { title = "Edit: Card " ++ selectedCard.number
        , buttons =
            [ Icon.print "Print card"
                |> Button.fromIcon
                |> Button.cmd (Msg.PrintCard selectedCard) Button.light
            ]
        , goBackMsg = Msg.DiscardCardChanges
        }
    <|
        cardEditView renderConfig selectedCard

-}
bodyStack : Stack msg -> Element msg -> PageBody msg
bodyStack prop body =
    Internal.PageBodyStack prop body


{-| `Nav.menuPage` describes a page to [`Nav.withMenuPages`](#withMenuPages).

    Nav.menuPage (Icon.edit "Edit cards")
        (Link.link "/edit-cards")
        (currentPage == Pages.CardsEdit)

-}
menuPage : Icon -> Link -> Bool -> MenuPage
menuPage icon link isCurrent =
    MenuPage <| Menu.Page icon link isCurrent


{-| `Nav.menuPage` describes an action to [`Nav.withMenuActions`](#withMenuActions).

    Nav.menuAction
        (Icon.logout "Logout")
        Msg.SessionLogout

-}
menuAction : Icon -> msg -> MenuAction msg
menuAction icon msg =
    MenuAction <| Menu.Action icon msg


{-| `Nav.navigator` holds the minimum amount of information required for all the features (menu, dialogs, and page's layout) to work.

The first parameter is a function that should transform [`Nav.Msg`](#Msg) in a message type controlled by the user.

The second is the current [`Nav.State`](#State), do not initialize this on view, hold it on the app's model, and then pass it to this function.

The third (and last) parameter is a lambda used to obtain the current page's container.

    Nav.Navigator Msg.FromNav
        model.navState
        (\page ->
            case page of
                Page.CardsEdit ->
                    Pages.CardsEdit.View.container

                Page.AccountProfile ->
                    Pages.AcountProfile.View.container
        )

-}
navigator :
    (Msg -> msg)
    -> State
    -> (page -> Page msg)
    -> Navigator page msg
navigator applier (State state) pagesPages =
    Navigator <|
        NavigatorRecord pagesPages
            (menu applier state)
            SidebarPersistent


{-| `Nav.dialog` constructs a [`Nav.Dialog`](#Dialog) from a title, a close message, and the dialog's view.
Note that the title renders inside the dialog's decoration, with a close button on the opposite side.

This dialog and its decoration render over the current page's view, adding a transparent black layer between them.
Clicking on the black layer also activates the closing message.

    Nav.dialog
        "Create a new card"
        Msg.NewCardDiscard
        (cardNewView model)

**Note**: This is deprecated, use 'Nav.dialogV2' instead

-}
dialog : String -> msg -> Element msg -> Dialog msg
dialog title onClose body =
    Dialog1
        { title = title
        , close = onClose
        , body = body
        }


{-|

    `Nav.dialogV2` constructs a [`Nav.Dialog`](#Dialog) from dialog v2.
    This variant of the dialog does not require you to specify body at the time
    of construction and you can specify it alongside buttons as an option
    like this:

    Dialogv2.dialog "An example dialog" Icon.warning closeMsg
        |> Dialogv2.withBody (Element.text "Dialog body")
        |> Dialogv2.withButtons [submitButton, cancleButton]
        |> dialogV2

-}
dialogV2 : Dialog2.Dialog msg -> Dialog msg
dialogV2 dlg =
    Dialog2 dlg



-- Render


{-| End of the builder's life.
The result of this function is a ready-to-use [`Browser.Document`](/packages/elm/browser/latest/Browser#Document).

There is an additional parameter that is the page identifier, used to obtain the current container.

    Nav.toBrowserDocument renderConfig currentPage navigator

-}
toBrowserDocument :
    RenderConfig
    -> page
    -> Navigator page msg
    -> { body : List (Html msg), title : String }
toBrowserDocument cfg page (Navigator model) =
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
                    SideBar.mobile cfg
                        contentBody
                        model.menu
                        seenTitle
                        maybeStack

                else
                    case model.sidebarStyle of
                        SidebarPersistent ->
                            SideBar.desktopPersistent cfg contentBody model.menu

                        SidebarNonPersistent ->
                            SideBar.desktopNonPersistent cfg contentBody model.menu

            else
                contentBody

        dialogView =
            case container.dialog of
                Just (Dialog1 dialogState) ->
                    Dialog1.view cfg dialogState

                Just (Dialog2 dialogState) ->
                    dialogState
                        |> dialogViewV2 cfg

                Nothing ->
                    Element.none

        bodyWithDialog =
            -- Always creating this element is required so we don't loose scrollbar state
            Element.el
                [ Element.inFront dialogView
                , Element.width fill
                , Element.height fill
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


menu : (Msg -> msg) -> StateRecord -> Menu.Menu msg
menu applier { menuExpanded } =
    Menu.default (ToggleMenu >> applier) menuExpanded


contentProps :
    String
    -> PageBody msg
    -> ( Element msg, Maybe ( msg, Maybe (Action.WithIcon msg) ), ( String, Maybe String ) )
contentProps mainTitle content =
    case content of
        Internal.PageBodySingle body ->
            ( body, Nothing, ( mainTitle, Nothing ) )

        Internal.PageBodyStack { title, goBackMsg, action } body ->
            ( body, Just ( goBackMsg, action ), title )


dialogMap : (a -> b) -> Dialog a -> Dialog b
dialogMap applier dlg =
    case dlg of
        Dialog1 dialogState ->
            Dialog1 <| Dialog1.dialogMap applier dialogState

        Dialog2 dialogState ->
            Dialog2 <| Dialog2.map applier dialogState

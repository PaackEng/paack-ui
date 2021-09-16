module UI.Document exposing
    ( Msg, Model, modelInit, modelWithClosedMenu, modelUpdate
    , Document, document
    , Page, page, pageWithDialog, pageWithDefaultMenu, pageMap
    , PageBody, bodySingle, Stack, bodyStack
    , withMenuLogo, withMenuActions, MenuAction, menuAction, withMenuPages
    , MenuPage, menuPage, withSidebarStyle, sidebarPersistent
    , sidebarNonPersistent, showMenu, hideMenu
    , toBrowserDocument
    )

{-| The `UI.Document` is paack-ui's page presenter and document.
Depending on the situation, it applies the sidebar menu, dialogs, and mobile's navbar over the current page.

For this, a page must provide some required data through the [`Document.Page`](#Page) record.
That aggregated with the current rendering configuration provided by [`UI.RenderConfig`](UI-RenderConfig#RenderConfig) and the [`Document.Model`](#Model), multiplex between possible viewing layouts.

Example of usage:

    view : RenderConfig -> Model.Model -> { body : List (Html Msg.Msg), title : String }
    view renderConfig { documentModel, currentPage } =
        Document.document Msg.DocumentMsg
            documentModel
            (getPagePage >> Document.pageMap Msg.PageMsg)
            |> Document.withMenuPages
                [ Document.menuPage (Icon.packages "Packages")
                    (Link.link "/packages")
                    (currentPage == Page.Packages)
                ]
            |> Document.withMenuActions
                [ Document.menuAction
                    (Icon.logout "Logout")
                    Msg.SessionLogout
                ]
            |> Document.withMenuLogo "My company's logo" someLogoElement
            |> Document.toBrowserDocument renderConfig currentPage

    getPagePage : Page.Page -> Document.Page Page.Msg
    getPagePage page =
        case page of
            Page.Packages pageModel ->
                Document.page "Packages"
                    (Document.bodySingle <| Packages.view pageModel)
                    |> Document.pageWithDialog pageModel.maybeDialog
                    |> Document.pageWithDefaultMenu


# Model & Update

@docs Msg, Model, modelInit, modelWithClosedMenu, modelUpdate


# Building

@docs Document, document


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
import UI.Dialog as Dialog exposing (Dialog)
import UI.Effects as Effects exposing (Effects)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Dialog as Dialog
import UI.Internal.Menu as Menu
import UI.Internal.Page as Internal
import UI.Internal.SideBar as SideBar
import UI.Link exposing (Link)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Utils.Action as Action



-- Expose


{-| This record must be generated with [`Document.menuPage`](#menuPage)
-}
type MenuPage
    = MenuPage Menu.Page


{-| This record must be generated with [`Document.menuAction`](#menuAction)
-}
type MenuAction msg
    = MenuAction (Menu.Action msg)



-- Types


{-| Keep this one in your Model, it holds the current navigation state.
-}
type Model
    = Model ModelRecord


type alias ModelRecord =
    { menuExpanded : Bool }


{-| The `Document.Msg` handles menu related messages.
-}
type Msg
    = ToggleMenu Bool


{-| The `Document.PageBody msg` manages different kinds of pages' body.
By now, the content is either a typical single page or a stacked child of mobile's views.

The typical single page renders the way they come.
The stacked child has a different header on mobile, where a back button replaces the sandwich button.

-}
type alias PageBody msg =
    Internal.PageBody msg


{-| The `Document.Page msg` describes the current page in its current state.

The `title` field is exposed as to the browser and reused on the mobile's navbar.

When it is `Just someElement`, the `dialog` field is shown over the page.

The `hasMenu` field can hide the menu when undesired, e.g., login page.

The `content` field must be the element holding the page's view.

    { content = Document.bodySingle <| Element.el [] [ Element.text "Element body" ]
    , title = "Example page"
    , dialog = Nothing -- or Just <| Document.dialog <| ...
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


{-| The `Document.Document` handles menu, dialogs and page viewing.
It must be initialized using [`Document.document`](#document).
-}
type Document pageSet msg
    = Document (DocumentRecord pageSet msg)


type alias DocumentRecord pageSet msg =
    { container : pageSet -> Page msg
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


{-| `Document.withMenuPages` replaces the list of the menu's navigable pages.
Therefore changing the list of items showed at the top of the sidebar.

    Document.withMenuPages
        [ Document.menuPage (Icon.edit "Manage cards")
            (Link.link "/edit-cards")
            (currentPage == Page.CardsEdit)
        , Document.menuPage (Icon.add "New manager")
            (Link.link "/add-manager")
            (currentPage == Page.ManagerAdd)
        ]
        someNav

-}
withMenuPages : List MenuPage -> Document page msg -> Document page msg
withMenuPages pages (Document nav) =
    let
        menuWithPages (Menu.Menu prop opt) =
            Menu.Menu prop { opt | pages = List.map (\(MenuPage p) -> p) pages }
    in
    Document { nav | menu = menuWithPages nav.menu }


{-| `Document.withMenuActions` replaces the list of the menu's actions.
Therefore changing the list of items showed at the bottom of the sidebar.

    Document.withMenuActions
        [ Document.menuAction
            (Icon.language "Change to English")
            (Msg.SetLanguage Lang.English)
        , Document.menuAction
            (Icon.logout "Logout")
            Msg.SessionLogout
        ]
        someNav

-}
withMenuActions : List (MenuAction msg) -> Document page msg -> Document page msg
withMenuActions actions (Document nav) =
    let
        menuWithActions (Menu.Menu prop opt) =
            Menu.Menu prop { opt | actions = List.map (\(MenuAction a) -> a) actions }
    in
    Document { nav | menu = menuWithActions nav.menu }


{-| `Document.withMenuLogo` replaces the logo shown on the menu.
By now, this logo is only visible at the desktop's sidebar.

The first parameter is a hint that exists for accessibility reasons.

    Document.withMenuLogo "Paack - Time Matters"
        Vectors.paackLogoWhite
        someNav

-}
withMenuLogo : String -> Element msg -> Document page msg -> Document page msg
withMenuLogo hint body (Document nav) =
    let
        menuWithLogo (Menu.Menu prop opt) =
            Menu.Menu prop
                { opt | logo = Just <| Menu.Logo hint body }
    in
    Document { nav | menu = menuWithLogo nav.menu }


{-| `Document.withSidebarStyle` takes a `SidebarStyle` setting the
appearance/behavior of the sidebar when it is enabled by the `hasMenu` flag.
-}
withSidebarStyle : SidebarStyle -> Document page msg -> Document page msg
withSidebarStyle style (Document nav) =
    Document { nav | sidebarStyle = style }


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


{-| Initializes a page's description

    Document.page "My page's title"
        (Document.bodySingle <| SomePage.view somePageModel)

-}
page : String -> PageBody msg -> Page msg
page title content =
    Page
        { title = title
        , content = content
        , hasMenu = False
        , dialog = Nothing
        }


{-| Overlay (or not) a dialog over the page.

    Document.pageWithDialog
        (Just <| Element.text "Hello World")
        someDocumentPage

-}
pageWithDialog : Maybe (Dialog msg) -> Page msg -> Page msg
pageWithDialog dialog (Page pageRecord) =
    Page { pageRecord | dialog = dialog }


{-| Make the document's default menu available in this page.

    Document.pageWithDefaultMenu
        someDocumentPage

-}
pageWithDefaultMenu : Page msg -> Page msg
pageWithDefaultMenu (Page pageRecord) =
    Page { pageRecord | hasMenu = True }


{-| Transform the messages produced by a container.
-}
pageMap : (a -> b) -> Page a -> Page b
pageMap applier (Page data) =
    Page
        { title = data.title
        , hasMenu = data.hasMenu
        , content = contentMap applier data.content
        , dialog = Maybe.map (Dialog.map applier) data.dialog
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


{-| Given a message, apply an update to the [`Document.Model`](#Model).
-}
modelUpdate : Msg -> Model -> ( Model, Cmd Msg )
modelUpdate msg state =
    modelUpdateWithoutPerform msg state
        |> Tuple.mapSecond Effects.perform


{-| Similar to [`modelUpdate`], but using Effects.
-}
modelUpdateWithoutPerform : Msg -> Model -> ( Model, Effects Msg )
modelUpdateWithoutPerform msg (Model state) =
    case msg of
        ToggleMenu newVal ->
            ( Model { state | menuExpanded = newVal }, Effects.none )



-- Constructors


{-| The default way of instantiating a [`Document.Model`](#Model).

    Document.modelInit renderConfig

-}
modelInit : RenderConfig -> Model
modelInit cfg =
    Model { menuExpanded = not <| RenderConfig.isMobile cfg }


{-| Force the menu to be closed on the [`Document.Model`](#Model).

    Document.modelInitWithClosedMenu renderConfig

-}
modelWithClosedMenu : Model -> Model
modelWithClosedMenu (Model state) =
    Model { state | menuExpanded = False }


{-| `Document.bodySingle` indicates that the current page is a simple single page.
It expects the final page's view in the only parameter.

    Document.bodySingle <| view renderConfig model

-}
bodySingle : Element msg -> PageBody msg
bodySingle body =
    Internal.PageBodySingle body


{-| `Document.bodyStack` indicates that the current page is a stack child's page.
It expects the child's configuration and the final page's view as its parameters.

    Document.bodyStack
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


{-| `Document.menuPage` describes a page to [`Document.withMenuPages`](#withMenuPages).

    Document.menuPage (Icon.edit "Edit cards")
        (Link.link "/edit-cards")
        (currentPage == Pages.CardsEdit)

-}
menuPage : Icon -> Link -> Bool -> MenuPage
menuPage icon link isCurrent =
    MenuPage <| Menu.Page icon link isCurrent


{-| `Document.menuPage` describes an action to [`Document.withMenuActions`](#withMenuActions).

    Document.menuAction
        (Icon.logout "Logout")
        Msg.SessionLogout

-}
menuAction : Icon -> msg -> MenuAction msg
menuAction icon msg =
    MenuAction <| Menu.Action icon msg


{-| `Document.document` holds the minimum amount of information required for all the features (menu, dialogs, and page's layout) to work.

The first parameter is a function that should transform [`Document.Msg`](#Msg) in a message type controlled by the user.

The second is the current [`Document.Model`](#Model), do not initialize this on view, hold it on the app's model, and then pass it to this function.

The third (and last) parameter is a lambda used to obtain the current page's container.

    Document.Document Msg.FromNav
        model.documentModel
        (\page ->
            case page of
                Page.CardsEdit ->
                    Pages.CardsEdit.View.container

                Page.AccountProfile ->
                    Pages.AcountProfile.View.container
        )

-}
document :
    (Msg -> msg)
    -> Model
    -> (pageSet -> Page msg)
    -> Document pageSet msg
document applier (Model state) pagesPages =
    Document <|
        DocumentRecord pagesPages
            (menu applier state)
            SidebarPersistent



-- Render


{-| End of the builder's life.
The result of this function is a ready-to-use [`Browser.Document`](/packages/elm/browser/latest/Browser#Document).

There is an additional parameter that is the page identifier, used to obtain the current container.

    Document.toBrowserDocument renderConfig currentPage document

-}
toBrowserDocument :
    RenderConfig
    -> pageSet
    -> Document pageSet msg
    -> { body : List (Html msg), title : String }
toBrowserDocument cfg pageItem (Document model) =
    let
        (Page container) =
            model.container pageItem

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
                Just dialog ->
                    Dialog.dialogView cfg dialog

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


menu : (Msg -> msg) -> ModelRecord -> Menu.Menu msg
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

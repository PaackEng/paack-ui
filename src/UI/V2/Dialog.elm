module UI.V2.Dialog exposing
    ( Dialog(..), dialog
    , withBody, withButtons
    , map
    )

{-| The `UI.V2.Dialog` is a component for displaying dialogs and modals.

User must specify a title and an icon to be displayed in the title to construct
it. Body and buttons can be specified optionally as in the following pipeline:

    dialog "Title" Icon.warning closeMsg
        |> withBody ("Body text" |> Text.body2 |> Text.renderElement cfg)
        |> withButtons buttons


# Building

@docs Dialog, dialog


# Content

@docs withBody, withButtons


# Component handling

@docs map

-}

import Element exposing (Element)
import UI.Button as Button exposing (Button)
import UI.Icon exposing (Icon)



-- Building


{-| The `Dialog msg` type is used for describing the component for later
rendering.
-}
type Dialog msg
    = Dialog Properties (Options msg)


type alias Properties =
    { title : String
    , icon : Icon
    }


type alias Options msg =
    { body : Element msg
    , buttons : List (Button msg)
    , overlayClickCloseMsg : Maybe msg
    }


{-| Constructs a dialog by receiving its title and an icon in the title.
-}
dialog : String -> Icon -> Dialog msg
dialog title icon =
    Dialog (Properties title icon) (Options Element.none [] Nothing)



-- Component handling


{-| Transforms the message produced by the component.
-}
map : (a -> b) -> Dialog a -> Dialog b
map applier (Dialog { title, icon } { body, buttons, overlayClickCloseMsg }) =
    Dialog
        { title = title
        , icon = icon
        }
        { body = Element.map applier body
        , buttons = List.map (Button.map applier) buttons
        , overlayClickCloseMsg = Maybe.map applier overlayClickCloseMsg
        }



-- Content


{-| With `Dialog.withBody` you can specify the body of the dialog.

**Note**: By default, the body is `Element.none`

-}
withBody : Element msg -> Dialog msg -> Dialog msg
withBody body (Dialog props options) =
    Dialog props { options | body = body }


{-| With `Dialog.withButtons` you can specify the buttons for the footer of the
dialog.

**Note**: By default, the buttons are an empty list

-}
withButtons : List (Button msg) -> Dialog msg -> Dialog msg
withButtons buttons (Dialog props options) =
    Dialog props { options | buttons = buttons }

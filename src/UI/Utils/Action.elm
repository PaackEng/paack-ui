module UI.Utils.Action exposing
    ( Action(..)
    , WithIcon, iconMap, iconWith
    )

{-| Describes a clickable element for components with customizable clickable action.


# Building

@docs Action


# Helpers

@docs WithIcon, iconMap, iconWith

-}

import UI.Icon exposing (Icon)
import UI.Link exposing (Link)


{-| `WithIcon` assembles the required configuration for having an action element.

    { label = "Create new element"
    , icon = Icon.add
    , action =
        DialogMsg.OpenElementCreation
            |> Msg.ForDialog
            |> Action.DispatchMsg
    }

-}
type alias WithIcon msg =
    { action : Action msg
    , icon : String -> Icon
    , label : String
    }


type Action msg
    = DispatchMsg msg
    | TriggerRedirect Link


iconMap : (a -> b) -> WithIcon a -> WithIcon b
iconMap applier old =
    case old.action of
        DispatchMsg oldMsg ->
            { action = DispatchMsg (applier oldMsg)
            , icon = old.icon
            , label = old.label
            }

        TriggerRedirect link ->
            { action = TriggerRedirect link
            , icon = old.icon
            , label = old.label
            }


iconWith : (Icon -> Icon) -> WithIcon msg -> WithIcon msg
iconWith applier old =
    { old | icon = \label -> applier (old.icon label) }

module UI.Utils.Action exposing
    ( Config, Action(..)
    , map, mapIcon
    )

{-| Describes a clickable element for components with customizable clickable action.


# Building

@docs Config, Action

-}

import UI.Icon exposing (Icon)
import UI.Link exposing (Link)


{-| `Config` assembles the required configuration for having an action element.

    { label = "Create new element"
    , icon = Icon.add
    , action =
        DialogMsg.OpenElementCreation
            |> Msg.ForDialog
            |> Action.DispatchMsg
    }

-}
type alias Config msg =
    { action : Action msg
    , icon : String -> Icon
    , label : String
    }


type Action msg
    = DispatchMsg msg
    | TriggerRedirect Link


map : (a -> b) -> Config a -> Config b
map applier old =
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


mapIcon : (Icon -> Icon) -> Config msg -> Config msg
mapIcon applier old =
    { old | icon = \label -> applier (old.icon label) }

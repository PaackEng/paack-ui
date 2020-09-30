module UI.Internal.Clickable exposing (actionIcon, actionWrapElement, msgWrapElement)

import Element exposing (Attribute, Element)
import UI.Icon as Icon exposing (Icon)
import UI.Link as Link
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA
import UI.Utils.Action as Action exposing (Action(..))
import UI.Utils.Element as Element


msgWrapElement : List (Attribute msg) -> msg -> Element msg -> Element msg
msgWrapElement attributes onClickMsg =
    [ Element.pointer
    , Element.onIndividualClick onClickMsg
    ]
        |> (++) (ARIA.toElementAttributes ARIA.roleButton)
        |> (++) attributes
        |> Element.el


actionIcon :
    RenderConfig
    -> List (Attribute msg)
    -> Action.Config msg
    -> Element msg
actionIcon renderConfig attributes { icon, label, action } =
    icon label
        |> Icon.renderElement renderConfig
        |> actionWrapElement renderConfig attributes action


actionWrapElement :
    RenderConfig
    -> List (Attribute msg)
    -> Action msg
    -> Element msg
    -> Element msg
actionWrapElement renderConfig attributes action child =
    case action of
        DispatchMsg onClickMsg ->
            msgWrapElement attributes onClickMsg child

        TriggerRedirect link ->
            Link.wrapElement renderConfig attributes link child

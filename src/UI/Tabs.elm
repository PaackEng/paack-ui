module UI.Tabs exposing
    ( TabList, tabList, actionTabList
    , renderElement
    )

{-| Tab allows visually selecting a page in a horizontal list.

Example of usage:

    TabList.tabList Msg.SportSelect
        Sports.toString
        [ Sports.Soccer
        , Sports.Basket
        , Sports.Bowling
        ]
        model.sportSelected
        |> TabList.renderElement renderConfig


# Building

@docs TabList, tabList, actionTabList


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, shrink)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import UI.Internal.Basics exposing (ifThenElse)
import UI.Internal.Colors as Colors
import UI.Link as Link
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA
import UI.Utils.Action as Action exposing (Action)
import UI.Utils.Element exposing (zeroPadding)


{-| The `TabList msg a` type is used for describing the component for later rendering.
-}
type TabList msg a
    = TabList (Properties msg a)


type alias Properties msg a =
    { action : a -> Action msg
    , label : a -> String
    , items : List a
    , current : a
    }


{-| Describes everything required to create a [`TabList msg a`](#TabList).

    Tabs.tabList Msg.TabSelect
        tabToString
        [ TabOne, TabTwo ]
        model.tabSelected

-}
tabList : (a -> msg) -> (a -> String) -> List a -> a -> TabList msg a
tabList selectMsg label items current =
    TabList
        { action = \item -> Action.DispatchMsg <| selectMsg item
        , label = label
        , items = items
        , current = current
        }


{-| Similar to [`tabList`](#tabList) but using [`Àction msg`](UI-Utils-Action#Action) instead.

    Tabs.actionTabList (tabToLink >> Action.TriggerRedirect)
        tabToString
        [ TabOne, TabTwo ]
        model.tabSelected

-}
actionTabList : (a -> Action msg) -> (a -> String) -> List a -> a -> TabList msg a
actionTabList action label items current =
    TabList
        { action = \item -> action item
        , label = label
        , items = items
        , current = current
        }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> TabList msg a -> Element msg
renderElement renderConfig (TabList { action, items, label, current }) =
    items
        |> List.map
            (\item ->
                itemView renderConfig
                    (action item)
                    (label item)
                    (current == item)
            )
        |> Element.row
            [ Element.spacing 20
            , Element.behindContent normalBorder

            -- Equivalent to Text.Body2 but with semiBold
            , Font.size 14
            , Font.letterSpacing 0.25
            , Font.semiBold
            ]


itemView : RenderConfig -> Action msg -> String -> Bool -> Element msg
itemView renderConfig action label isCurrent =
    let
        attrs =
            buttonAttributes isCurrent
    in
    case action of
        Action.DispatchMsg selectMsg ->
            Input.button attrs
                { onPress =
                    if isCurrent then
                        Nothing

                    else
                        Just selectMsg
                , label = itemLabel label isCurrent
                }

        Action.TriggerRedirect link ->
            Link.wrapElement renderConfig attrs link <|
                itemLabel label isCurrent


itemLabel : String -> Bool -> Element msg
itemLabel label isCurrent =
    label
        |> Element.text
        |> Element.el (labelBaseAttrs isCurrent)


buttonAttributes : Bool -> List (Attribute msg)
buttonAttributes isCurrent =
    if isCurrent then
        ARIA.roleTab True
            |> ARIA.toElementAttributes
            |> (::) (Element.focused [])

    else
        ARIA.toElementAttributes <| ARIA.roleTab False


labelBaseAttrs : Bool -> List (Attribute msg)
labelBaseAttrs isCurrent =
    [ Element.paddingEach { zeroPadding | bottom = 9 }
    , Element.width shrink
    , Font.color
        (ifThenElse isCurrent currentColor textNormalColor)
    , Element.pointer
    , Element.inFront
        (ifThenElse isCurrent currentBorder Element.none)
    ]


currentBorder : Element msg
currentBorder =
    Element.el
        [ Border.widthEach { zeroPadding | bottom = 1 }
        , Border.color currentColor
        , Element.width fill
        , Element.alignBottom
        ]
        Element.none


normalBorder : Element msg
normalBorder =
    Element.el
        [ Border.widthEach { zeroPadding | bottom = 1 }
        , Border.color borderNormalColor
        , Element.width fill
        , Element.alignBottom
        ]
        Element.none


textNormalColor : Element.Color
textNormalColor =
    Colors.gray600


borderNormalColor : Element.Color
borderNormalColor =
    Colors.gray300


currentColor : Element.Color
currentColor =
    Colors.navyBlue700

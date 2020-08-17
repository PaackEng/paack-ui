module UI.Tabs exposing
    ( Item, fromList
    , Tabs, cmd, redirect
    , renderElement
    )

{-| Tab allows visually selecting a page in a horizontal list.

This does not includes the logic of detecting current page, neither the logic to replace the contents.

Example of usage:

    Tabs.fromList
        [ Tab.redirect "Soccer"
            (Link.link "/soccer/results")
            (currentPage == Page.SoccerResults)
        , Tab.cmd "Basketball"
            Msg.GoToBasketball
            (currentPage == Page.BasketballResults)
        , Tab.redirect "Bowling"
            (Link.link "/bowl/results")
            (currentPage == Page.BowlingResults)
        ]
        |> Tabs.renderElement renderConfig


# Building

@docs Item, fromList


# Items

@docs Tabs, cmd, redirect


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Decoration, Element, fill, shrink)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import UI.Internal.Basics exposing (ifThenElse)
import UI.Internal.Palette as Palette
import UI.Internal.Utils.Element as Element
import UI.Link as Link exposing (Link)
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element exposing (zeroPadding)


{-| The `Tabs msg` type is used for describing the component for later rendering.
-}
type Tabs msg
    = Tabs (List (Item msg))


{-| A single element in a tab list.
-}
type Item msg
    = Item ItemConfig (ItemAction msg)


type alias ItemConfig =
    { label : String
    , isCurrent : Bool
    }


type ItemAction msg
    = ActionRedirect Link
    | ActionMsg msg


{-| Transform a list of [`Item msg`](#Item) in a [`Tabs msg`](#Tabs).
-}
fromList : List (Item msg) -> Tabs msg
fromList list =
    Tabs list


{-| When this tab is selected the user is redirected to a new page.

    Tabs.redirect "News"
        (Link.link "/news")
        (currentPage == Page.News)

-}
redirect : String -> Link -> Bool -> Item msg
redirect label link isCurrent =
    Item { label = label, isCurrent = isCurrent } <| ActionRedirect link


{-| When this tab is selected a message is triggered.

    Tabs.cmd "News"
        (Msg.TabSelect TabNews)
        (currentTab == TabNews)

-}
cmd : String -> msg -> Bool -> Item msg
cmd label onClick isCurrent =
    Item { label = label, isCurrent = isCurrent } <| ActionMsg onClick


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Tabs msg -> Element msg
renderElement renderConfig (Tabs list) =
    list
        |> List.map (itemView renderConfig)
        |> Element.row
            [ Element.spacing 20
            , Element.behindContent normalBorder

            -- Equivalent to Text.Body2 but with semiBold
            , Font.size 14
            , Font.letterSpacing 0.25
            , Font.semiBold
            ]


itemView : RenderConfig -> Item msg -> Element msg
itemView renderConfig (Item { label, isCurrent } action) =
    case action of
        ActionMsg onClick ->
            Input.button (focusNotCurrent isCurrent)
                { onPress = Just onClick
                , label = itemLabel label isCurrent
                }

        ActionRedirect link ->
            Input.button (focusNotCurrent isCurrent)
                { onPress = Nothing
                , label = Link.wrapElement renderConfig [] link <| itemLabel label isCurrent
                }


itemLabel : String -> Bool -> Element msg
itemLabel label isCurrent =
    label
        |> Element.text
        |> Element.el (labelBaseAttrs isCurrent)


focusNotCurrent : Bool -> List (Attribute msg)
focusNotCurrent isCurrent =
    if isCurrent then
        [ Element.focused [] ]

    else
        []


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
        ++ (ARIA.toElementAttributes <| ARIA.roleTab isCurrent)


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
    Palette.gray.light


borderNormalColor : Element.Color
borderNormalColor =
    Palette.gray.lighter


currentColor : Element.Color
currentColor =
    Palette.primary.middle

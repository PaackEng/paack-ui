module UI.Tabs exposing (Item, Tabs, cmd, fromList, redirect, renderElement)

{-| Example of usage:

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

-}

import Element exposing (Attribute, Element, fill, shrink)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Internal.Basics exposing (ifThenElse)
import UI.Internal.Palette as Palette
import UI.Link as Link exposing (Link)
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.Element exposing (zeroPadding)


type Tabs msg
    = Tabs (List (Item msg))


type Item msg
    = Item ItemConfig (ItemAction msg)


type alias ItemConfig =
    { label : String
    , isCurrent : Bool
    }


type ItemAction msg
    = ActionRedirect Link
    | ActionMsg msg


fromList : List (Item msg) -> Tabs msg
fromList list =
    Tabs list


redirect : String -> Link -> Bool -> Item msg
redirect label link isCurrent =
    Item { label = label, isCurrent = isCurrent } <| ActionRedirect link


cmd : String -> msg -> Bool -> Item msg
cmd label onClick isCurrent =
    Item { label = label, isCurrent = isCurrent } <| ActionMsg onClick


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
    itemLabel label isCurrent
        |> (case action of
                ActionMsg onClick ->
                    Element.el [ Events.onClick onClick ]

                ActionRedirect link ->
                    Link.wrapElement renderConfig [] link
           )


itemLabel : String -> Bool -> Element msg
itemLabel label isCurrent =
    label
        |> Element.text
        |> Element.el (labelBaseAttrs isCurrent)


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
    Palette.gray.light


borderNormalColor : Element.Color
borderNormalColor =
    Palette.gray.lighter


currentColor : Element.Color
currentColor =
    Palette.primary.middle

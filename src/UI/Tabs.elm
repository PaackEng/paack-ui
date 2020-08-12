module UI.Tabs exposing (Tabs, TabsItem, fromList, page, renderElement)

{-| Example of usage:

    Tabs.fromList
        [ Tab.page "Soccer"
            (Link.link "/soccer/results")
            (currentPage == Page.SoccerResults)
        , Tab.page "Basketball"
            (Link.link "/basket/results")
            (currentPage == Page.BasketballResults)
        , Tab.page "Bowling"
            (Link.link "/bowl/results")
            (currentPage == Page.BowlingResults)
        ]
        |> Tabs.renderElement renderConfig

-}

import Element exposing (Element)


type Tabs msg
    = -- We have "msg" for future usage.
      Tabs (List TabsItem)


type TabsItem
    = Page PageConfig


type alias PageConfig =
    { label : String
    , link : Link
    , isCurrent : Bool
    }


fromList : List TabsItem -> Tabs msg
fromList list =
    Tabs list


page : String -> Link -> Bool -> TabsItem
page label link isCurrent =
    Page (PageConfig label link isCurrent)


renderElement : RenderConfig -> Tabs msg -> Element msg
renderElement cfg (Tabs list) =
    Element.none

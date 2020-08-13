module Tabs.Stories exposing (stories, update)

import Element exposing (Element)
import Msg
import PluginOptions exposing (defaultWithMenu)
import Return as R exposing (Return)
import Tabs.Model as Stories
import Tabs.Msg as Stories
import UI.RenderConfig exposing (RenderConfig)
import UI.Tabs as Tabs
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyWithModel)


update : Stories.Msg -> Stories.Model -> Return Stories.Msg Stories.Model
update msg model =
    case msg of
        Stories.Select newPage ->
            ( { model | selected = newPage }, Cmd.none )


stories renderConfig =
    storiesOf
        "Tabs"
        [ artistsStory renderConfig
        ]


artistsStory renderConfig =
    storyWithModel
        ( "Tabs"
        , \{ tabsStories } -> artistsTabs renderConfig tabsStories.selected
        , { defaultWithMenu
            | note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tabs) for the exact code of this example."
          }
        )


artistsTabs : RenderConfig -> Stories.TabsDemo -> Element Msg.Msg
artistsTabs renderConfig selected =
    Tabs.fromList
        [ Tabs.cmd "About"
            (select Stories.About)
            (selected == Stories.About)
        , Tabs.cmd "Corey Taylor"
            (select Stories.CoreyTaylor)
            (selected == Stories.CoreyTaylor)
        , Tabs.cmd "Matt Bellamy"
            (select Stories.MattBellamy)
            (selected == Stories.MattBellamy)
        , Tabs.cmd "Dave Ghrowl"
            (select Stories.DaveGhrowl)
            (selected == Stories.DaveGhrowl)
        , Tabs.cmd "Brandon Flowers"
            (select Stories.BrandonFlowers)
            (selected == Stories.BrandonFlowers)
        , Tabs.cmd "Julian Casablancas"
            (select Stories.JulianCasablancas)
            (selected == Stories.JulianCasablancas)
        , Tabs.cmd "Chris Martin"
            (select Stories.ChrisMartin)
            (selected == Stories.ChrisMartin)
        , Tabs.cmd "Alex Turner"
            (select Stories.AlexTurner)
            (selected == Stories.AlexTurner)
        , Tabs.cmd "Gerard Way"
            (select Stories.GerardWay)
            (selected == Stories.GerardWay)
        ]
        |> Tabs.renderElement renderConfig


select =
    Stories.Select >> Msg.TabsStoriesMsg

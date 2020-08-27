module Tabs.Stories exposing (stories, update)

import Element exposing (Element)
import Msg exposing (Msg)
import Return exposing (Return)
import Tabs.Model as Stories
import Tabs.Msg as Stories
import UI.RenderConfig exposing (RenderConfig)
import UI.Tabs as Tabs
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, reducedToDocs, storyWithModel)


update : Stories.Msg -> Stories.Model -> Return Stories.Msg Stories.Model
update msg model =
    case msg of
        Stories.Select newPage ->
            ( { model | selected = newPage }, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Tabs"
        [ artistsStory renderConfig
        ]


artistsStory : RenderConfig -> ExplorerStory
artistsStory renderConfig =
    storyWithModel
        ( "Tabs"
        , \{ tabsStories } -> artistsTabs renderConfig tabsStories.selected
        , reducedToDocs "Tabs"
        )


artistsTabs : RenderConfig -> Stories.TabsDemo -> Element Msg.Msg
artistsTabs renderConfig selected =
    Tabs.tabList select
        Stories.toString
        [ Stories.About
        , Stories.CoreyTaylor
        , Stories.MattBellamy
        , Stories.DaveGrohl
        , Stories.BrandonFlowers
        , Stories.JulianCasablancas
        , Stories.ChrisMartin
        , Stories.AlexTurner
        , Stories.GerardWay
        ]
        selected
        |> Tabs.renderElement renderConfig


select : Stories.TabsDemo -> Msg
select =
    Stories.Select >> Msg.TabsStoriesMsg

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
    Tabs.tabList select
        toString
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


select =
    Stories.Select >> Msg.TabsStoriesMsg


toString item =
    case item of
        Stories.About ->
            "About"

        Stories.CoreyTaylor ->
            "Corey Taylor"

        Stories.MattBellamy ->
            "Matt Bellamy"

        Stories.DaveGrohl ->
            "Dave Grohl"

        Stories.BrandonFlowers ->
            "Brandon Flowers"

        Stories.JulianCasablancas ->
            "Julian Casablancas"

        Stories.ChrisMartin ->
            "Chris Martin"

        Stories.AlexTurner ->
            "Alex Turner"

        Stories.GerardWay ->
            "Gerard Way"

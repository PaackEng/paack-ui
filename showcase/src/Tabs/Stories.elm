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
        [ tab selected "About" Stories.About
        , tab selected "Corey Taylor" Stories.CoreyTaylor
        , tab selected "Matt Bellamy" Stories.MattBellamy
        , tab selected "Dave Ghrowl" Stories.DaveGhrowl
        , tab selected "Brandon Flowers" Stories.BrandonFlowers
        , tab selected "Julian Casablancas" Stories.JulianCasablancas
        , tab selected "Chris Martin" Stories.ChrisMartin
        , tab selected "Alex Turner" Stories.AlexTurner
        , tab selected "Gerard Way" Stories.GerardWay
        ]
        |> Tabs.renderElement renderConfig


select =
    Stories.Select >> Msg.TabsStoriesMsg


tab selected name item =
    Tabs.cmd name
        (select item)
        (selected == item)

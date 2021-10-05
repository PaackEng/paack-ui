module Tabs.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Msg exposing (Msg)
import Return exposing (Return)
import Tabs.Model as Stories
import Tabs.Msg as Stories
import UI.Link as Link
import UI.RenderConfig exposing (RenderConfig)
import UI.Tabs as Tabs
import UI.Utils.Action as Action exposing (Action)
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
        [ normalStory renderConfig
        , actionStory renderConfig
        , unitedStory renderConfig
        ]


normalStory : RenderConfig -> ExplorerStory
normalStory renderConfig =
    storyWithModel
        ( "Normal Tabs"
        , \{ tabsStories } -> artistsTabs renderConfig tabsStories.selected
        , reducedToDocs "Tabs"
        )


actionStory : RenderConfig -> ExplorerStory
actionStory renderConfig =
    storyWithModel
        ( "Action Tabs"
        , always <| redirectTabs renderConfig RedirectTabStory
        , reducedToDocs "Tabs"
        )


unitedStory : RenderConfig -> ExplorerStory
unitedStory renderConfig =
    storyWithModel
        ( "United"
        , \{ tabsStories } ->
            Element.column [ Element.width fill, Element.spacing 16 ]
                [ redirectTabs renderConfig UnitedStory
                , artistsTabs renderConfig tabsStories.selected
                ]
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


type RedirectTabs
    = NormalTabStory
    | RedirectTabStory
    | UnitedStory


redirectTabsToString : RedirectTabs -> String
redirectTabsToString tab =
    case tab of
        NormalTabStory ->
            "Normal Tabs"

        RedirectTabStory ->
            "Redirect Tabs"

        UnitedStory ->
            "United"


redirectTabsToAction : RedirectTabs -> Action msg
redirectTabsToAction tab =
    Action.TriggerRedirect <|
        case tab of
            NormalTabStory ->
                Link.link "#Basics/Tabs/Normal Tabs"

            RedirectTabStory ->
                Link.link "#Basics/Tabs/Action Tabs"

            UnitedStory ->
                Link.link "#Basics/Tabs/United"


redirectTabs : RenderConfig -> RedirectTabs -> Element Msg.Msg
redirectTabs renderConfig selected =
    Tabs.actionTabList
        redirectTabsToAction
        redirectTabsToString
        [ NormalTabStory, RedirectTabStory, UnitedStory ]
        selected
        |> Tabs.renderElement renderConfig


select : Stories.TabsDemo -> Msg
select =
    Stories.Select >> Msg.TabsStoriesMsg

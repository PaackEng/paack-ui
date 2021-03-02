module ContentPlaceholders exposing (stories)

import Element exposing (Element, fill)
import PluginOptions exposing (defaultWithMenu)
import UI.ContentPlaceholder as ContentPlaceholder exposing (ContentPlaceholder)
import UI.Icon as Icon
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , goToDocsCallToAction
        , iconsSvgSprite
        , mobileRenderConfig
        , prettifyElmCode
        , story
        )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "ContentPlaceholder"
        [ desktopStory renderConfig
        , mobileStory
        , customStory renderConfig
        , unitedStory renderConfig
        ]


desktopStory : RenderConfig -> ExplorerStory
desktopStory renderConfig =
    story
        ( "Nothing to see here (Dekstop)"
        , nothingToSeeHereView renderConfig
            |> withIconSpreadsheet
        , { defaultWithMenu
            | code = nothingToSeeHereCode
            , note = goToDocsCallToAction "ContentPlaceholder"
          }
        )


mobileStory : ExplorerStory
mobileStory =
    story
        ( "Nothing to see here (Mobile)"
        , nothingToSeeHereView mobileRenderConfig
            |> withIconSpreadsheet
        , { defaultWithMenu
            | code = nothingToSeeHereCode
            , note = goToDocsCallToAction "ContentPlaceholder"
          }
        )


nothingToSeeHereCode : String
nothingToSeeHereCode =
    prettifyElmCode """
    ContentPlaceholder.nothingToSeeHereView
        |> ContentPlaceholder.withLargeSize
        |> ContentPlaceholder.renderElement renderConfig
"""


customStory : RenderConfig -> ExplorerStory
customStory renderConfig =
    story
        ( "Custom"
        , customView renderConfig
            |> withIconSpreadsheet
        , { defaultWithMenu
            | code = customCode
            , note = goToDocsCallToAction "ContentPlaceholder"
          }
        )


nothingToSeeHereView : RenderConfig -> Element msg
nothingToSeeHereView renderConfig =
    Element.row [ Element.width fill ]
        [ genericView "Large"
            renderConfig
            True
            ContentPlaceholder.nothingToSeeHere
        , genericView "Medium"
            renderConfig
            False
            ContentPlaceholder.nothingToSeeHere
        ]


customView : RenderConfig -> Element msg
customView renderConfig =
    let
        customState =
            ContentPlaceholder.custom
                { icon = Icon.fix
                , title = "Select A Group"
                , body = "Please select a group to fix from the list on the left."
                }
    in
    Element.row [ Element.width fill ]
        [ genericView "Large/Desktop"
            renderConfig
            True
            customState
        , genericView "Large/Mobile"
            mobileRenderConfig
            True
            customState
        ]


genericView : String -> RenderConfig -> Bool -> ContentPlaceholder -> Element msg
genericView label renderConfig isLarge component =
    let
        applyLargeSize =
            if isLarge then
                ContentPlaceholder.withLargeSize

            else
                identity
    in
    Element.column [ Element.width fill ]
        [ Text.heading2 label
            |> Text.renderElement renderConfig
        , component
            |> applyLargeSize
            |> ContentPlaceholder.renderElement renderConfig
        ]


customCode : String
customCode =
    prettifyElmCode """
    ContentPlaceholder.custom
        { icon = Icon.fix
        , title = "Select A Group"
        , body = "Please select a group to fix from the list on the left."
        }
        |> ContentPlaceholder.withLargeSize
        |> ContentPlaceholder.renderElement renderConfig
"""


withIconSpreadsheet : Element msg -> Element msg
withIconSpreadsheet element =
    Element.column [ Element.width fill ]
        [ iconsSvgSprite
        , element
        ]


unitedStory : RenderConfig -> ExplorerStory
unitedStory renderConfig =
    story
        ( "United"
        , Element.column [ Element.width fill ]
            [ iconsSvgSprite
            , customView renderConfig
            , nothingToSeeHereView renderConfig
            , nothingToSeeHereView mobileRenderConfig
            ]
        , defaultWithMenu
        )

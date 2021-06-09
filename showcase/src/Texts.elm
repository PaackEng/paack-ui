module Texts exposing (stories)

import Element exposing (Element)
import Html exposing (Html)
import PluginOptions exposing (defaultWithoutMenu)
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text exposing (Text)
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerModel, ExplorerUI, goToDocsCallToAction, prettifyElmCode)


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Texts"
        [ ( "Texts"
          , textsView renderConfig
          , { defaultWithoutMenu
                | code = code
                , note = goToDocsCallToAction "Text"
            }
          )
        ]


code : String
code =
    prettifyElmCode """
"Wherever You Will Go"
    |> Text.heading5
    |> Text.withColor (Palette.color Palette.toneGray Palette.brightnessDarkest)
    |> Text.renderElement renderConfig

" - By The Calling"
    |> Text.caption
    |> Text.renderElement renderConfig

\"\"\"
So lately, been wondering
Who will be there to take my place?
\"\"\"
    |> Text.body1
    |> Text.renderElement renderConfig
"""


mobileRenderConfig : RenderConfig
mobileRenderConfig =
    RenderConfig.init
        { width = 340
        , height = 640
        }
        RenderConfig.localeEnglish


styles : List ( String -> Text, String )
styles =
    [ ( Text.heading1, "Heading 1" )
    , ( Text.heading2, "Heading 2" )
    , ( Text.heading3, "Heading 3" )
    , ( Text.heading4, "Heading 4" )
    , ( Text.heading5, "Heading 5" )
    , ( Text.heading6, "Heading 6" )
    , ( Text.subtitle1, "Subtitle 1" )
    , ( Text.subtitle2, "Subtitle 2" )
    , ( Text.body1, "Body 1" )
    , ( Text.body2, "Body 2" )
    , ( Text.caption, "Caption" )
    , ( Text.overline, "OVERLINE" )
    ]


styleView : RenderConfig -> ( String -> Text, String ) -> Element msg
styleView renderConfig ( component, label ) =
    label
        |> component
        |> Text.renderElement renderConfig


stylesView : RenderConfig -> String -> Element msg
stylesView renderConfig title =
    Element.column [ Element.width Element.fill, Element.spacing 40 ]
        [ Text.overline title
            |> Text.withColor Palette.primaryLight1
            |> Text.renderElement renderConfig
        , styles
            |> List.map (styleView renderConfig)
            |> Element.column
                [ Element.spacing 20
                ]
        ]


textsView : RenderConfig -> ExplorerModel -> Html msg
textsView renderConfig _ =
    Element.layout [ Element.width Element.fill ] <|
        Element.column [ Element.width Element.fill, Element.spacing 80 ]
            [ Element.wrappedRow [ Element.width Element.fill ]
                [ stylesView renderConfig "DESKTOP"
                , stylesView mobileRenderConfig "MOBILE"
                ]
            , Element.column
                [ Element.width (Element.px 400)
                , Element.spacing 20
                ]
                [ Text.heading3 "Example" |> Text.renderElement renderConfig
                , Text.body1 "While working on the Tron: Legacy soundtrack in 2010, Daft Punk met with Casablancas in their studio through a mutual friend. The duo, who are fans of Casablancas' band The Strokes, presented him with an instrumental demo track intended for use on Daft Punk's next album. Casablancas responded favorably to the demo upon listening to it and subsequently agreed to provide accompanying vocals, forming the basis for what would become \"Instant Crush\"."
                    |> Text.renderElement renderConfig
                , Text.body2 "\"I Will Follow You into the Dark\" is a song by indie\n            rock band Death Cab for Cutie, the third single from their fifth\n            album Plans, released on August 30, 2005. Written and performed by\n            Ben Gibbard, it is an acoustic solo ballad, and was recorded in\n            monaural with a single microphone and little editing."
                    |> Text.renderElement renderConfig
                , Text.caption "Still From \"Elephant Gun\" video, photo by: Kristianna Smith"
                    |> Text.renderElement renderConfig
                ]
            ]

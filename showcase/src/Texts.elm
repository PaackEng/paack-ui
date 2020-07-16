module Texts exposing (stories)

import Element
import UI.Palette as Palette exposing (brightnessMiddle, tonePrimary)
import UI.Text as Text
import UIExplorer exposing (storiesOf)


stories renderConfig =
    storiesOf
        "Texts"
        [ ( "Texts"
          , textsView renderConfig
          , { note = """
```elm
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
```

Futures discussions includes:
* How we'll add responsives attributes (font-size may variate with device size).
* * Maybe `Text.renderElement <device>`
* is `Text.toEl` really necessary?
""" }
          )
        ]


styles =
    [ ( Text.heading1
            >> Text.withColor (Palette.color tonePrimary brightnessMiddle)
      , "Is it working?"
      )
    , ( Text.heading2, "Heading2" )
    , ( Text.heading3, "Heading3" )
    , ( Text.heading4, "Heading4" )
    , ( Text.heading5, "Heading5" )
    , ( Text.heading6, "Heading6" )
    , ( Text.subtitle1, "Subtitle1" )
    , ( Text.subtitle2, "Subtitle2" )
    , ( Text.body1, "Body1: While working on the Tron: Legacy soundtrack in 2010, Daft Punk met with Casablancas in their studio through a mutual friend. The duo, who are fans of Casablancas' band The Strokes, presented him with an instrumental demo track intended for use on Daft Punk's next album. Casablancas responded favorably to the demo upon listening to it and subsequently agreed to provide accompanying vocals, forming the basis for what would become \"Instant Crush\"." )
    , ( Text.body2, "Body2: \"I Will Follow You into the Dark\" is a song by indie rock band Death Cab for Cutie, the third single from their fifth album Plans, released on August 30, 2005. Written and performed by Ben Gibbard, it is an acoustic solo ballad, and was recorded in monaural with a single microphone and little editing." )
    , ( Text.caption, "Caption: Still From \"Elephant Gun\" video, photo by: Kristianna Smith" )
    , ( Text.overline, "OVERLINE" )
    ]


styleView renderConfig ( component, label ) =
    label
        |> component
        |> Text.renderElement renderConfig


textsView renderConfig content =
    styles
        |> List.map (styleView renderConfig)
        |> Element.column
            [ Element.spacing 20
            ]
        |> Element.layout []

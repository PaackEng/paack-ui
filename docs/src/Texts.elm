module Texts exposing (stories)

import Element
import Element.Border as Border
import Element.Font as Font
import UI.Text as Text
import UIExplorer exposing (storiesOf)


stories =
    storiesOf
        "Texts"
        [ ( "Texts"
          , \_ -> textsView
          , { note = "" }
          )
        ]


styles =
    [ ( Text.heading1, "Heading1" )
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


styleView ( component, label ) =
    label
        |> component
        |> Text.toEl


textsView =
    styles
        |> List.map styleView
        |> Element.column
            [ Element.spacing 20
            , Font.family [ Font.typeface "Inter", Font.sansSerif ]
            ]
        |> Element.layout []

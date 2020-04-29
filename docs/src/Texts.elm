module Texts exposing (stories)

import Element
import Element.Border as Border
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
    , ( Text.body1, "Body1" )
    , ( Text.body2, "Body2" )
    , ( Text.caption, "Caption" )
    , ( Text.overline, "Overline" )
    ]


styleView ( component, label ) =
    label
        |> component
        |> Text.toEl


textsView =
    styles
        |> List.map styleView
        |> Element.column
            [ Element.spacing 10
            ]
        |> Element.layout []

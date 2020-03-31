module Icons exposing (stories)

import Element
import Element.Background as Background
import Element.Font as Font
import UI.Icons as Icons
import UI.Theme as Theme
import UIExplorer exposing (storiesOf)


stories =
    storiesOf
        "Icons"
        [ ( "IconsExample"
          , \_ -> iconsView
          , { note = "" }
          )
        ]


icons =
    [ ( Icons.chevronUp, "Chevron Up" )
    , ( Icons.chevronDown, "Chevron Down" )
    , ( Icons.chevronLeft, "Chevron Left" )
    , ( Icons.chevronRight, "Chevron Right" )
    , ( Icons.close, "Close" )
    , ( Icons.collapse, "Collapse" )
    , ( Icons.expand, "Expand" )
    , ( Icons.filter, "Filter" )
    , ( Icons.map, "Map" )
    , ( Icons.mapPoint, "Map Point" )
    , ( Icons.messages, "Messages" )
    , ( Icons.more, "More" )
    , ( Icons.move, "Move" )
    , ( Icons.notepad, "Notepad" )
    , ( Icons.person, "Person" )
    , ( Icons.plus, "Plus" )
    , ( Icons.reload, "Reload" )
    , ( Icons.search, "Search" )
    , ( Icons.settings, "Settings" )
    , ( Icons.trash, "Trash" )
    , ( Icons.checked, "Checked" )
    , ( Icons.unchecked, "Unchecked" )
    , ( Icons.warning, "Warning" )
    ]


iconView ( iconFn, label ) =
    Element.column
        [ Background.color Theme.primary
        , Font.color Theme.white
        , Element.spacing 10
        , Element.padding 10
        ]
        [ Element.el
            [ Font.size 25
            , Element.centerX
            ]
            (iconFn label)
        , Element.el [ Font.size 14 ] <| Element.text label
        ]


iconsView =
    icons
        |> List.map iconView
        |> Element.wrappedRow
            [ Element.spacing 10
            ]
        |> Element.layout []

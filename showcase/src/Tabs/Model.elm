module Tabs.Model exposing (Model, TabsDemo(..), initModel, toString)


type TabsDemo
    = About
    | CoreyTaylor
    | MattBellamy
    | DaveGrohl
    | BrandonFlowers
    | JulianCasablancas
    | ChrisMartin
    | AlexTurner
    | GerardWay


type alias Model =
    { selected : TabsDemo
    }


initModel : Model
initModel =
    { selected = About
    }


toString : TabsDemo -> String
toString item =
    case item of
        About ->
            "About"

        CoreyTaylor ->
            "Corey Taylor"

        MattBellamy ->
            "Matt Bellamy"

        DaveGrohl ->
            "Dave Grohl"

        BrandonFlowers ->
            "Brandon Flowers"

        JulianCasablancas ->
            "Julian Casablancas"

        ChrisMartin ->
            "Chris Martin"

        AlexTurner ->
            "Alex Turner"

        GerardWay ->
            "Gerard Way"

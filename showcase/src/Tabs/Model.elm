module Tabs.Model exposing (Model, TabsDemo(..), initModel)


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

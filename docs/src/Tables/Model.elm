module Tables.Model exposing (Model, Player, initModel, players)

import UI.Table as Table


type alias Model =
    { defaultTable : Table.State Player String
    }


type alias Player =
    { number : Int
    , name : String
    , position : String
    }


initModel : Model
initModel =
    { defaultTable = Table.initState .name
    }


players : List Player
players =
    [ Player 23 "Lebron James" "Small Foward"
    , Player 35 "Kevin Durant" "Power Forward"
    , Player 0 "Kevin Love" "Center"
    , Player 12 "Jouderian Nobre" "Power Foward"
    , Player 41 "Dirk Nowitzki" "PowerForward"
    , Player 7 "Carmelo Anthony" "Small Foward"
    ]

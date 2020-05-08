module UI.Internal.Menu exposing (..)

import Element exposing (Element)
import UI.Icon as Icon exposing (Icon)
import UI.Link as Link exposing (Link)


type Menu msg
    = Menu (Properties msg) (Options msg)


type alias Properties msg =
    { toggleMsg : Bool -> msg
    , isExpanded : Bool
    }


type alias Options msg =
    { pages : List Page
    , actions : List (Action msg)
    , logo : Maybe (Logo msg)
    }


type alias Page =
    { labeledIcon : Icon
    , link : Link
    , isCurrent : Bool
    }


type alias Logo msg =
    { hint : String
    , body : Element msg
    }


type alias Action msg =
    { labeledIcon : Icon
    , action : msg
    }


default : (Bool -> msg) -> Bool -> Menu msg
default toggle isExpanded =
    Menu
        (Properties toggle isExpanded)
        { pages = []
        , actions = []
        , logo = Nothing
        }

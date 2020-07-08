module UI.Utils.TypeNumbers exposing (..)


type alias Zero =
    Never


type Increase a
    = Increase a


type alias One =
    Increase Zero


type alias Two =
    Increase One


type alias Three =
    Increase Two


type alias Four =
    Increase Three


type alias Five =
    Increase Four


type alias Six =
    Increase Five


type alias Seven =
    Increase Six


type alias Eight =
    Increase Seven


type alias Nive =
    Increase Eight

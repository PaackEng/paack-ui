module UI.Badge exposing (dark, error, light, primary, success, warning)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Theme as Theme


type alias ColorScheme =
    { font : Color
    , background : Color
    }


light : String -> Element msg
light =
    view { font = Theme.black, background = Theme.gray3 }


primary : String -> Element msg
primary =
    view { font = Theme.white, background = Theme.primary }


dark : String -> Element msg
dark =
    view { font = Theme.white, background = Theme.black }


success : String -> Element msg
success =
    view { font = Theme.black, background = Theme.success }


warning : String -> Element msg
warning =
    view { font = Theme.black, background = Theme.warning }


error : String -> Element msg
error =
    view { font = Theme.black, background = Theme.error }


view : ColorScheme -> String -> Element msg
view { font, background } t =
    el
        [ Background.color background
        , Font.color font
        , Theme.regular
        , Border.rounded 20
        , paddingXY 10 6
        ]
        (text t)

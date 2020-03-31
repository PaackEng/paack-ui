module UI.Input.Checkbox exposing
    ( Checkbox
    , checkbox
    , toEl
    , withDisabledMode
    , withIsChecked
    , withLabel
    )

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import Helpers exposing (ifThenElse)
import UI.Attributes exposing (ariaChecked, ariaDisabled, ariaLabel, ariaRole, onClickWithoutPropagation, style, title)
import UI.Icons as Icons
import UI.Theme as Theme


type Checkbox msg
    = Checkbox (Options msg)


type alias Options msg =
    { onChange : Bool -> msg
    , label : String
    , isDisabled : Bool
    , isChecked : Bool
    }


checkbox : (Bool -> msg) -> Checkbox msg
checkbox msg =
    Checkbox
        { onChange = msg
        , label = ""
        , isDisabled = False
        , isChecked = False
        }


withIsChecked : Bool -> Checkbox msg -> Checkbox msg
withIsChecked isChecked (Checkbox options) =
    Checkbox
        { options | isChecked = isChecked }


withDisabledMode : Bool -> Checkbox msg -> Checkbox msg
withDisabledMode isDisabled (Checkbox options) =
    Checkbox
        { options | isDisabled = isDisabled }


withLabel : String -> Checkbox msg -> Checkbox msg
withLabel label (Checkbox options) =
    Checkbox
        { options | label = label }


toEl : Checkbox msg -> Element msg
toEl (Checkbox options) =
    let
        iconType =
            ifThenElse options.isChecked Icons.checked Icons.unchecked

        extraAttrs =
            if options.isDisabled then
                [ Background.color Theme.gray4
                , Font.color Theme.gray2
                ]

            else
                [ Font.color Theme.primary
                , onClickWithoutPropagation <| options.onChange (not options.isChecked)
                ]

        attrs =
            [ ariaRole "checkbox"
            , ariaChecked options.isChecked
            , ariaLabel options.label
            , title options.label
            , pointer
            , Region.announce
            , ariaDisabled options.isDisabled
            ]
                |> List.append extraAttrs
    in
    el attrs
        (iconType "")

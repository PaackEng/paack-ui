module UI.Events exposing (onEnter)

import Element exposing (Attribute)
import Html.Events as Events
import Json.Decode as Decode


onEnter : msg -> Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )

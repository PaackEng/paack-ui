module Tests.Utils.ExtraEvents exposing (buttonEnterKey, inputEnterKey)

import Json.Encode as Encode exposing (Value)
import Test.Html.Event as Event


buttonEnterKey : ( String, Value )
buttonEnterKey =
    Event.custom "keydown" <|
        Encode.object [ ( "key", Encode.string "Enter" ) ]


inputEnterKey : ( String, Value )
inputEnterKey =
    Event.custom "keyup" <|
        Encode.object [ ( "key", Encode.string "Enter" ) ]

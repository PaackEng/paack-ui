module Tests.Utils.ExtraEvents exposing (enterKey)

import Json.Encode as Encode exposing (Value)
import Test.Html.Event as Event


enterKey : ( String, Value )
enterKey =
    Event.custom "keyup" <|
        Encode.object [ ( "key", Encode.string "Enter" ) ]

module UI.Internal.NavigationContainer exposing (Content(..), StackChild, toShowcaseElement)

import Element exposing (Element)
import UI.Button exposing (Button, ButtonStyle)
import UI.Text exposing (Text)


type Content msg
    = ContentSingle (Element msg)
    | ContentStackChild (StackChild msg) (Element msg)


type alias StackChild msg =
    { title : Text
    , buttons : List (ButtonStyle -> Button msg)
    , goBackMsg : msg
    }


toShowcaseElement : Content msg -> Element msg
toShowcaseElement content =
    case content of
        ContentSingle body ->
            body

        ContentStackChild _ body ->
            body

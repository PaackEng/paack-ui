module UI.Internal.NavigationContainer exposing (Content(..), StackChild, toShowcaseElement)

import Element exposing (Element)
import UI.Utils.Action as Action


type Content msg
    = ContentSingle (Element msg)
    | ContentStackChild (StackChild msg) (Element msg)


type alias StackChild msg =
    { title : ( String, Maybe String )
    , action : Maybe (Action.WithIcon msg)
    , goBackMsg : msg
    }


toShowcaseElement : Content msg -> Element msg
toShowcaseElement content =
    case content of
        ContentSingle body ->
            body

        ContentStackChild _ body ->
            body

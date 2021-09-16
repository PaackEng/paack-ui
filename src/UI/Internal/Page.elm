module UI.Internal.Page exposing (PageBody(..), Stack, toShowcaseElement)

import Element exposing (Element)
import UI.Utils.Action as Action


type PageBody msg
    = PageBodySingle (Element msg)
    | PageBodyStack (Stack msg) (Element msg)


type alias Stack msg =
    { title : ( String, Maybe String )
    , action : Maybe (Action.WithIcon msg)
    , goBackMsg : msg
    }


toShowcaseElement : PageBody msg -> Element msg
toShowcaseElement content =
    case content of
        PageBodySingle body ->
            body

        PageBodyStack _ body ->
            body

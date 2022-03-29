module UI.Internal.Page exposing (PageBody(..), Stack)

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

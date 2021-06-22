module Radio.Msg exposing (Msg(..))

import Browser.Dom as Dom
import Radio.Model as Model


type Msg
    = Set String Model.Options
    | FocusResult (Result Dom.Error ())
    | NoOp

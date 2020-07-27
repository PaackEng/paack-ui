module UI.Internal.RenderConfig exposing (Locale(..), RenderConfig(..), RenderConfigData, string, strings)

import Element
import I18n.English as LangEnglish
import I18n.Spanish as LangSpanish
import I18n.Types as I18n


type alias RenderConfigData =
    { deviceClass : Element.DeviceClass
    , deviceOrientation : Element.Orientation
    , locale : Locale
    }


{-| `RenderConfig.RenderConfig` upholds all the information required for the components to apply the responsivity and accessibility changes.
-}
type RenderConfig
    = RenderConfig RenderConfigData


type Locale
    = EnglishGB
    | SpanishES


string : (I18n.Root -> String) -> RenderConfig -> String
string getter cfg =
    getter (strings cfg)


strings : RenderConfig -> I18n.Root
strings (RenderConfig { locale }) =
    case locale of
        EnglishGB ->
            LangEnglish.root

        SpanishES ->
            LangSpanish.root

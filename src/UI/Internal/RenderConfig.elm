module UI.Internal.RenderConfig exposing (Locale(..), RenderConfig(..), RenderConfigData, localeTerms)

import Element
import I18n.English as LangEnglish
import I18n.Spanish as LangSpanish
import I18n.Types as I18n


type alias RenderConfigData =
    { deviceClass : Element.DeviceClass
    , deviceOrientation : Element.Orientation
    , locale : Locale
    }


type RenderConfig
    = RenderConfig RenderConfigData


type Locale
    = EnglishGB
    | SpanishES


localeTerms : RenderConfig -> I18n.Root
localeTerms (RenderConfig { locale }) =
    case locale of
        EnglishGB ->
            LangEnglish.root

        SpanishES ->
            LangSpanish.root

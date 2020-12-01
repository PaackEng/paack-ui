module UI.Internal.RenderConfig exposing (Locale(..), RenderConfig(..), RenderConfigData, localeTerms)

import Element
import I18n.English as LangEnglish
import I18n.French as LangFrench
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
    = English
    | Spanish
    | French


localeTerms : RenderConfig -> I18n.Root
localeTerms (RenderConfig { locale }) =
    case locale of
        English ->
            LangEnglish.root

        Spanish ->
            LangSpanish.root

        French ->
            LangFrench.root

module UI.Internal.RenderConfig exposing (Locale(..), RenderConfig(..), RenderConfigData, localeTerms)

import Element
import UI.I18n.English as LangEnglish
import UI.I18n.French as LangFrench
import UI.I18n.Portuguese as LangPortuguese
import UI.I18n.Spanish as LangSpanish
import UI.I18n.Types as I18n


type alias RenderConfigData =
    { deviceClass : Element.DeviceClass
    , deviceOrientation : Element.Orientation
    , locale : Locale
    }


type RenderConfig
    = RenderConfig RenderConfigData


type Locale
    = English
    | French
    | Portuguese
    | Spanish


localeTerms : RenderConfig -> I18n.Root
localeTerms (RenderConfig { locale }) =
    case locale of
        English ->
            LangEnglish.root

        French ->
            LangFrench.root

        Portuguese ->
            LangPortuguese.root

        Spanish ->
            LangSpanish.root

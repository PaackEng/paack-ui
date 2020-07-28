module Tests.Utils.RenderConfig exposing (desktopWindowConfig)

import UI.RenderConfig as RenderConfig exposing (RenderConfig)


desktopWindowConfig : RenderConfig
desktopWindowConfig =
    RenderConfig.init
        { width = 1920
        , height = 1080
        }
        RenderConfig.localeEnglish

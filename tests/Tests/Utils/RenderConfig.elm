module Tests.Utils.RenderConfig exposing (desktopWindowConfig)

import UI.RenderConfig exposing (RenderConfig)


desktopWindowConfig : RenderConfig
desktopWindowConfig =
    UI.RenderConfig.fromWindow
        { width = 1920
        , height = 1080
        }

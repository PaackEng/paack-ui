module UI.RenderConfig exposing (RenderConfig, fromWindow, isMobile, isPortrait)

{- -- Future thoughts:

   type RenderStyle
      = StyleLight
      | StyleDark
      | StyleAutoOnSunset -- Too utopic, better to never do

   withStyle: RenderStyle -> RenderConfig -> RenderConfig
   isDark : DateTime -> RenderConfig -> Bool
-}

import Element


type alias RenderConfigData =
    { deviceClass : Element.DeviceClass
    , deviceOrientation : Element.Orientation
    }


type RenderConfig
    = RenderConfig RenderConfigData


fromWindow : { window | height : Int, width : Int } -> RenderConfig
fromWindow window =
    let
        { class, orientation } =
            Element.classifyDevice window
    in
    { deviceClass = class
    , deviceOrientation = orientation
    }
        |> RenderConfig


isMobile : RenderConfig -> Bool
isMobile (RenderConfig { deviceClass }) =
    deviceClass == Element.Phone


isPortrait : RenderConfig -> Bool
isPortrait (RenderConfig { deviceOrientation }) =
    deviceOrientation == Element.Portrait

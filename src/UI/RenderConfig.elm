module UI.RenderConfig exposing (RenderConfig, elLayoutAttributes, fromWindow, isMobile, isPortrait)

{- -- Future thoughts:

   type RenderStyle
      = StyleLight
      | StyleDark
      | StyleAutoOnSunset -- Too utopic, better to never do

   withStyle: RenderStyle -> RenderConfig -> RenderConfig
   isDark : DateTime -> RenderConfig -> Bool
-}

import Element exposing (Attribute)
import Element.Font as Font


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


elLayoutAttributes : RenderConfig -> List (Attribute msg)
elLayoutAttributes _ =
    -- Why here? Accessibility settings may change fonts, backgrounds, etc...
    [ Font.family [ Font.typeface "Inter", Font.sansSerif ] ]

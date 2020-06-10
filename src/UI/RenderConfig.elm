module UI.RenderConfig exposing
    ( RenderConfig
    , elLayoutAttributes
    , fromWindow
    , isMobile
    , isPortrait
    , updateWindow
    )

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
        ( class, orientation ) =
            classifyWindow window
    in
    { deviceClass = class
    , deviceOrientation = orientation
    }
        |> RenderConfig


updateWindow : { window | height : Int, width : Int } -> RenderConfig -> RenderConfig
updateWindow window (RenderConfig oldData) =
    let
        ( class, orientation ) =
            classifyWindow window
    in
    { oldData
        | deviceClass = class
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


classifyWindow : { window | height : Int, width : Int } -> ( Element.DeviceClass, Element.Orientation )
classifyWindow window =
    let
        { orientation } =
            Element.classifyDevice window

        class =
            if window.width < 600 then
                Element.Phone

            else
                Element.Desktop
    in
    ( class, orientation )

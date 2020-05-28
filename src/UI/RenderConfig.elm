module UI.RenderConfig exposing
    ( ContextualSize(..)
    , RenderConfig
    , elLayoutAttributes
    , fromWindow
    , getContextualSize
    , isMobile
    , isPortrait
    , updateWindow
    , withContextualSize
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
    , contextSize : Maybe ContextualSize
    }


type ContextualSize
    = SizeExtraLarge -- 48px
    | SizeLarge -- 40px
    | SizeSmall -- 24px


type RenderConfig
    = RenderConfig RenderConfigData


getContextualSize : RenderConfig -> ContextualSize
getContextualSize (RenderConfig data) =
    data.contextSize |> Maybe.withDefault SizeExtraLarge


fromWindow : { window | height : Int, width : Int } -> RenderConfig
fromWindow window =
    let
        { class, orientation } =
            Element.classifyDevice window
    in
    { deviceClass = class
    , deviceOrientation = orientation
    , contextSize = Nothing
    }
        |> RenderConfig


updateWindow : { window | height : Int, width : Int } -> RenderConfig -> RenderConfig
updateWindow window (RenderConfig oldData) =
    let
        { class, orientation } =
            Element.classifyDevice window
    in
    { oldData
        | deviceClass = class
        , deviceOrientation = orientation
    }
        |> RenderConfig


withContextualSize : ContextualSize -> RenderConfig -> RenderConfig
withContextualSize size (RenderConfig data) =
    RenderConfig { data | contextSize = Just size }


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

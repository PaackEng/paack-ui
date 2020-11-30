module UI.RenderConfig exposing
    ( RenderConfig, init
    , Locale, localeEnglish, localeSpanish, localeFrench
    , updateWindow, updateLocale
    , isMobile, isPortrait
    , elLayoutAttributes
    )

{-| `UI.RenderConfig` propagates data for accessibility and responsivity in all components.

The recommended approach is storing it in the app's Model, gathering the required init parameters using Elm's flags. For keeping it update, subscribe to [`Browser.onResize`](/packages/elm/browser/latest/Browser-Events#onResize) and apply update using [`RenderConfig.updateWindow`](UI-RenderConfig#updateWindow).

    RenderConfig.init
        { width = flags.innerWidth
        , height = flags.innerHeight
        }
        RenderConfig.localeEnglish


# Building

@docs RenderConfig, init


# Locales

@docs Locale, localeEnglish, localeSpanish, localeFrench


# Update

@docs updateWindow, updateLocale


# Responsivity

@docs isMobile, isPortrait


# Layout

@docs elLayoutAttributes

-}

import Element exposing (Attribute)
import Element.Font as Font
import UI.Internal.RenderConfig as Internal


{-| `RenderConfig.RenderConfig` upholds all the information required for the components to apply the responsivity and accessibility changes.
-}
type alias RenderConfig =
    Internal.RenderConfig


{-| Define how to alterate text and dates to fit a localization profile.
-}
type alias Locale =
    Internal.Locale


{-| Equivalent to en-US according to ISO 639.1 and ISO 3166.
-}
localeEnglish : Locale
localeEnglish =
    Internal.English


{-| Equivalent to es-ES according to ISO 639.1 and ISO 3166.
-}
localeSpanish : Locale
localeSpanish =
    Internal.Spanish


{-| Equivalent to fr-FR according to ISO 639.1 and ISO 3166.
-}
localeFrench : Locale
localeFrench =
    Internal.French


{-| `RenderConfig.init` builds a [`RenderConfig.RenderConfig`](UI-RenderConfig#RenderConfig) with the minimum necessary information for responsivity to work.

    RenderConfig.init
        { width = 1920
        , height = 1080
        }
        RenderConfig.localeEnglish

-}
init : { window | height : Int, width : Int } -> Locale -> RenderConfig
init window locale =
    let
        ( class, orientation ) =
            classifyWindow window
    in
    { deviceClass = class
    , deviceOrientation = orientation
    , locale = locale
    }
        |> Internal.RenderConfig


{-| The subscribed event of resizing the browser should reflect on a `RenderConfig` update.
For that, use `RenderConfig.updateWindow`

    RenderConfig.updateWindow
        { width = 1920
        , height = 1080
        }
        oldRenderConfig

-}
updateWindow : { window | height : Int, width : Int } -> RenderConfig -> RenderConfig
updateWindow window (Internal.RenderConfig oldData) =
    let
        ( class, orientation ) =
            classifyWindow window
    in
    { oldData
        | deviceClass = class
        , deviceOrientation = orientation
    }
        |> Internal.RenderConfig


{-| The subscribed event of resizing the browser should reflect on a `RenderConfig` update.
For that, use `RenderConfig.updateWindow`

    RenderConfig.updateWindow
        { width = 1920
        , height = 1080
        }
        oldRenderConfig

-}
updateLocale : Locale -> RenderConfig -> RenderConfig
updateLocale locale (Internal.RenderConfig oldData) =
    { oldData
        | locale = locale
    }
        |> Internal.RenderConfig


{-| `True` when the browser is a mobile browser.

    if RenderConfig.isMobile renderConfig then
        -- Mobile view
    else
        -- Desktop view

-}
isMobile : RenderConfig -> Bool
isMobile (Internal.RenderConfig { deviceClass }) =
    deviceClass == Element.Phone


{-| `True` when the browser is in portrait orientation and not landscape.

    if RenderConfig.isPortrait renderConfig then
        -- Portrait view
    else
        -- Landscape view

-}
isPortrait : RenderConfig -> Bool
isPortrait (Internal.RenderConfig { deviceOrientation }) =
    deviceOrientation == Element.Portrait


{-| `RenderConfig.elLayoutAttributes` are the recommended attributes for [`Element.layout`](/packages/mdgriffith/elm-ui/latest/Element#layout).

If the app is using [`UI.NavigationContainer.navigator`](UI-NavigationContainer#navigator) then there is nothing to worry about as it already uses these attributes.

    Element.layout (RenderConfig.elLayoutAttributes renderConfig) (appView model)

-}
elLayoutAttributes : RenderConfig -> List (Attribute msg)
elLayoutAttributes _ =
    -- Why here? Accessibility settings may change fonts, backgrounds, etc...
    [ Font.family [ Font.typeface "Inter", Font.sansSerif ] ]



-- Internal


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

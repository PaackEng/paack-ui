module UI.RenderConfig exposing
    ( RenderConfig, fromWindow
    , updateWindow
    , isMobile, isPortrait
    , elLayoutAttributes
    )

{-| `UI.RenderConfig` propagates data for accessibility and responsivity in all components.

The recommended approach is storing it in the app's Model, gathering the required init parameters using Elm's flags. For keeping it update, subscribe to [`Browser.onResize`](/packages/elm/browser/latest/Browser-Events#onResize) and apply update using [`RenderConfig.updateWindow`](UI-RenderConfig#updateWindow).

    RenderConfig.fromWindow
        { width = flags.innerWidth
        , height = flags.innerHeight
        }


# Building

@docs RenderConfig, fromWindow


# Update

@docs updateWindow


# Responsivity

@docs isMobile, isPortrait


# Layout

@docs elLayoutAttributes

-}

import Element exposing (Attribute)
import Element.Font as Font


type alias RenderConfigData =
    { deviceClass : Element.DeviceClass
    , deviceOrientation : Element.Orientation
    }


{-| `RenderConfig.RenderConfig` upholds all the information required for the components to apply the responsivity and accessibility changes.
-}
type RenderConfig
    = RenderConfig RenderConfigData


{-| `RenderConfig.fromWindow` builds a [`RenderConfig.RenderConfig`](UI-RenderConfig#RenderConfig) with the minimum necessary information for responsivity to work.

    RenderConfig.fromWindow
        { width = 1920
        , height = 1080
        }

-}
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


{-| The subscribed event of resizing the browser should reflect on a `RenderConfig` update.
For that, use `RenderConfig.updateWindow`

    RenderConfig.updateWindow
        { width = 1920
        , height = 1080
        }
        oldRenderConfig

-}
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


{-| `True` when the browser is a mobile browser.

    if RenderConfig.isMobile renderConfig then
        -- Mobile view
    else
        -- Desktop view

-}
isMobile : RenderConfig -> Bool
isMobile (RenderConfig { deviceClass }) =
    deviceClass == Element.Phone


{-| `True` when the browser is in portrait orientation and not landscape.

    if RenderConfig.isPortrait renderConfig then
        -- Portrait view
    else
        -- Landscape view

-}
isPortrait : RenderConfig -> Bool
isPortrait (RenderConfig { deviceOrientation }) =
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

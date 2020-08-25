# Paack UI

The paack-ui package provides ready-to-use components using Elm UI, based on the design sketches elaborated for Paack's apps.

The components' API strikes to be strict. That means it pragmatically stops the developer from performing undesired options combinations. Thus, producing unexpected behaviors or non-planned visuals artifacts.

The purpose of using Elm UI is mostly to keep components accessible.

The usage of this library is mostly internal, but we are open to suggestions and feedback.

## Example

Here's an example of this package in action:

```elm
module Pages.Login.View exposing (container)

import Element exposing (Element, fill, maximum, minimum, shrink)
import UI.Button as Button
import UI.NavigationContainer as Nav
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.TextField as TextField
-- and others


container : AppConfig -> Model -> Nav.Container Msg
container appConfig model =
    { title = pageTitle appConfig.locale model
    , content = Nav.contentSingle <| viewBody appConfig model
    , dialog = Nothing
    , hasMenu = False
    }


loginForm : Locale -> RenderConfig -> Model -> Element Msg
loginForm ({ t } as locale) cfg model =
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacingXY 8 24
        , Element.padding 32
        ]
        [ TextField.username SetEmail  -- UI.TextField
            (t "pages.login.username")
            model.email
            |> TextField.withPlaceholder "jon@paack.co"
            |> TextField.setLabelVisible True
            |> TextField.withWidth TextField.widthFull
            |> TextField.renderElement cfg
        , TextField.currentPassword SetPassword -- UI.TextField
            (t "pages.login.password")
            model.password
            |> TextField.withPlaceholder "********"
            |> TextField.setLabelVisible True
            |> TextField.withWidth TextField.widthFull
            |> TextField.withOnEnterPressed GetCredentials
            |> TextField.renderElement cfg -- UI.TextField
        , Button.fromLabel (t "pages.login.login") -- UI.Button
            |> Button.cmd GetCredentials Button.primary
            |> Button.renderElement cfg
        ]


loginTitle : Locale -> RenderConfig -> Element Msg
loginTitle { t } cfg =
    t "pages.login.description"
        |> String.split "\n"
        |> Text.multiline Text.heading5 -- UI.Text
        |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
        |> Text.renderElement cfg
        |> Element.el
            [ Element.paddingXY 0 20
            , Element.width fill
            ]
```

## Showcase

This module's repository includes a showcase that itself is not an example of the module's usage but demonstrates the components and their appearance.

A published version of the showcase is available at [paackeng.github.io/paack-ui](https://paackeng.github.io/paack-ui).

## Assets

An external repository holds static assets used within this API.

For including these assets, we recommend using parcel.
First, add `paack-ui-assets` using `npm install`.
Secondly, add `import 'paack-ui-assets/js/paackSvgIconSprite.js';` to `index.js`

## Where to begin?

See [the `UI.NavigationContainer` module documentation](http://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-NavigationContainer) for the top-level component.

There is a showcase demo available. Run `npm showcase` and navigate to the pointed link. In this demo, you can see all different components in actions with some code samples.
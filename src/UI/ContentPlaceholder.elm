module UI.ContentPlaceholder exposing
    ( ContentPlaceholder
    , custom
    , nothingToSeeHere
    , withLargeSize
    , renderElement
    )

{-| `UI.ContentPlaceholders` fills space in an area that is waiting for some action from the user.

An content-placeholder can be created and rendered as in the following pipeline:

    ContentPlaceholders.custom
        { icon = Icon.fix
        , title = "Select A Group"
        , body = "Please select a group to fix from the list on the left."
        }
        |> ContentPlaceholders.withSize Size.large
        |> ContentPlaceholders.renderElement renderConfig

There are also some ready-to-use costructors.


# Building

@docs ContentPlaceholder
@docs custom


# Common values

@docs nothingToSeeHere


# Size

@docs withLargeSize


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill, maximum)
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Internal.Size exposing (Size(..))
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element as Element


{-| The `ContentPlaceholder` type is used for describing the component for later rendering.
-}
type ContentPlaceholder
    = Custom Properties Options
    | NothingToSeeHere Options


type alias Properties =
    { icon : String -> Icon
    , title : String
    , body : String
    }


type alias Options =
    { size : Size }


{-| Constructs a generic `ContentPlaceholder`.

    ContentPlaceholders.custom
        { icon = Icon.download
        , title = "Pick a file"
        , body = "Please select a file to download it."
        }
        |> ContentPlaceholders.renderElement renderConfig

-}
custom :
    { icon : String -> Icon
    , title : String
    , body : String
    }
    -> ContentPlaceholder
custom prop =
    Custom prop defaultOptions


{-| One of the ready-to-use constructors.
Used in impossible-states and empty unmanaged lists.

    ContentPlaceholders.nothingToSeeHere
        |> ContentPlaceholders.renderElement renderConfig

-}
nothingToSeeHere : ContentPlaceholder
nothingToSeeHere =
    NothingToSeeHere defaultOptions


defaultOptions : Options
defaultOptions =
    { size = Medium }


{-| With `withSize`, you'll be able to scale the icon and text between the [standard sizes][size].

[size]: UI-Size

    ContentPlaceholders.withSize Size.medium somePlaceholder

**NOTE**: Default value is [`Size.medium`](UI-Size#medium).

-}
withSize : Size -> ContentPlaceholder -> ContentPlaceholder
withSize newSize component =
    setOption (\opt -> { opt | size = newSize }) component


{-| With `withLargeSize`, you'll have a larger `ContentPlaceholder`.

    ContentPlaceholders.withLargeSize somePlaceholder

**NOTE**: Default size is [`Size.medium`](UI-Size#medium).

-}
withLargeSize : ContentPlaceholder -> ContentPlaceholder
withLargeSize component =
    withSize Large component


setOption : (Options -> Options) -> ContentPlaceholder -> ContentPlaceholder
setOption applier component =
    case component of
        Custom prop opt ->
            Custom prop <| applier opt

        NothingToSeeHere opt ->
            NothingToSeeHere <| applier opt


getProp : RenderConfig -> ContentPlaceholder -> Properties
getProp renderConfig component =
    case component of
        Custom prop _ ->
            prop

        NothingToSeeHere _ ->
            let
                { title, body } =
                    (localeTerms renderConfig).contentPlaceholders.nothingToSeeHere
            in
            { icon = Icon.configure
            , title = title
            , body = body
            }


getOpt : ContentPlaceholder -> Options
getOpt component =
    case component of
        Custom _ opt ->
            opt

        NothingToSeeHere opt ->
            opt


{-| End of the builder's life.
The result of component function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> ContentPlaceholder -> Element msg
renderElement renderConfig component =
    let
        { title, icon, body } =
            getProp renderConfig component

        { size } =
            getOpt component

        isLarge =
            size == Large

        titleText =
            case ( isLarge, RenderConfig.isMobile renderConfig ) of
                ( True, True ) ->
                    Text.heading5

                ( False, True ) ->
                    Text.heading6

                ( True, False ) ->
                    Text.heading4

                ( False, False ) ->
                    Text.heading5

        ( bodyText, iconPadding, iconSize ) =
            if isLarge then
                ( Text.body1, largeIconPadding, 96 )

            else
                ( Text.body2, mediumIconPadding, 80 )
    in
    Element.column
        [ Element.width (fill |> maximum 300)
        , Element.centerX
        , Element.centerY
        , Element.spacing 8
        , Font.center
        ]
        [ icon title
            |> Icon.withCustomSize iconSize
            |> Icon.withColor Palette.gray600
            |> Icon.renderElement renderConfig
            |> Element.el
                [ Element.centerX
                , Element.paddingEach iconPadding
                ]
        , titleText title
            |> Text.renderElement renderConfig
        , bodyText body
            |> Text.renderElement renderConfig
        ]


mediumIconPadding : Element.RectangleSides
mediumIconPadding =
    { top = 24
    , left = 0
    , right = 0
    , bottom = 12
    }


largeIconPadding : Element.RectangleSides
largeIconPadding =
    { top = 22
    , left = 0
    , right = 0
    , bottom = 14
    }

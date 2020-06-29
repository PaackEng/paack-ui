module UI.Link exposing
    ( Link, link
    , LinkTarget, withTarget, targetNew, targetRedirect
    , wrapElement
    )

{-| `UI.Link` is just a unified-type for both `Element.link` and \`Element.newTabLink

For a pratical example see [`Link.wrapElement`](UI-Link#wrapElement).


# Building

@docs Link, link


# Target

@docs LinkTarget, withTarget, targetNew, targetRedirect


# Rendering

@docs wrapElement

-}

import Element exposing (Attribute, Element)
import UI.RenderConfig exposing (RenderConfig)


type alias Options =
    { target : LinkTarget }


type alias Properties =
    { href : String }


{-| The `Link msg` type is used for describing the component for later rendering.
-}
type Link
    = Link Properties Options


{-| The `LinkTarget` is equivalent to [HTML anchor's target](https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement/target).
-}
type LinkTarget
    = TargetRedirect
    | TargetNew


{-| Builds a link to some desired URL path.

    Link.link "https://www.googgle.com"

-}
link : String -> Link
link href =
    Link (Properties href) (Options TargetRedirect)


{-| `LinkTarget` value for opening the link in the same tab.

    Link.withTarget Link.targetRedirect someLink

**NOTE**: This is the default behaviour.

-}
targetRedirect : LinkTarget
targetRedirect =
    TargetRedirect


{-| `LinkTarget` value for opening the link in a new tab.

    Link.withTarget Link.targetNew someLink

-}
targetNew : LinkTarget
targetNew =
    TargetNew


{-| Modify the link behaviour to open the URL in the same tab or a new one.

    Link.link "https://www.googgle.com"
        |> Link.withTarget Link.targetNew

-}
withTarget : LinkTarget -> Link -> Link
withTarget target (Link prop _) =
    Link prop (Options target)



-- Render


{-| `Link.wrapElement` is similar to [`Element.el`][el] but including the anchor linking behaviour.

[el]: /packages/mdgriffith/elm-ui/latest/Element#el

    Link.wrapElement renderConfig
        [ Element.centerX, Element.centerY ]
        (Link.link "https://www.github.com")
        someOtherElement

-}
wrapElement :
    RenderConfig
    -> List (Attribute msg)
    -> Link
    -> Element msg
    -> Element msg
wrapElement _ attrs (Link { href } { target }) subElement =
    let
        thisLink =
            case target of
                TargetRedirect ->
                    Element.link

                TargetNew ->
                    Element.newTabLink
    in
    thisLink attrs
        { url = href
        , label = subElement
        }

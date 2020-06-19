module UI.Link exposing
    ( Link
    , LinkTarget
    , link
    , targetNew
    , targetRedirect
    , withTarget
    , wrapElement
    )

import Element exposing (Attribute, Element)
import UI.RenderConfig exposing (RenderConfig)


type alias Options =
    { target : LinkTarget }


type alias Properties =
    { href : String }


type Link
    = Link Properties Options


type LinkTarget
    = TargetRedirect
    | TargetNew


link : String -> Link
link href =
    Link (Properties href) (Options TargetRedirect)


targetRedirect : LinkTarget
targetRedirect =
    TargetRedirect


targetNew : LinkTarget
targetNew =
    TargetNew


withTarget : LinkTarget -> Link -> Link
withTarget target (Link prop _) =
    Link prop (Options target)



-- Render


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

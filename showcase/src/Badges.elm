module Badges exposing (stories)

import PluginOptions exposing (defaultWithMenu)
import UI.Badge as Badge
import UIExplorer exposing (storiesOf)
import Utils exposing (goToDocsCallToAction, prettifyElmCode, story)


stories cfg =
    storiesOf
        "Badges"
        [ oneBadge cfg Badge.grayLight "Gray Light"
        , oneBadge cfg Badge.primaryLight "Primary Light"
        , oneBadge cfg Badge.warningLight "Warning Light"
        , oneBadge cfg Badge.dangerLight "Danger Light"
        , oneBadge cfg Badge.successLight "Success Light"
        , oneBadge cfg Badge.grayDark "Gray Dark"
        , oneBadge cfg Badge.primaryDark "Primary Dark"
        , oneBadge cfg Badge.warningDark "Warning Dark"
        , oneBadge cfg Badge.dangerDark "Danger Dark"
        , oneBadge cfg Badge.successDark "Success Dark"
        ]


oneBadge cfg constructor str =
    story
        ( "Badge " ++ str
        , Badge.renderElement cfg <| constructor "123"
        , { defaultWithMenu
            | code = prettifyElmCode <| """
Badge.""" ++ str ++ """ "123"
  |> Badge.renderElement renderCfg
"""
            , note = goToDocsCallToAction "Badge"
          }
        )

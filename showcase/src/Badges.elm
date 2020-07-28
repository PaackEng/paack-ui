module Badges exposing (stories)

import PluginOptions exposing (defaultWithMenu)
import UI.Badge as Badge
import UIExplorer exposing (storiesOf)
import Utils exposing (goToDocsCallToAction, prettifyElmCode, story)


stories cfg =
    storiesOf
        "Badges"
        [ oneBadge cfg Badge.grayLight "grayLight"
        , oneBadge cfg Badge.primaryLight "primaryLight"
        , oneBadge cfg Badge.warningLight "warningLight"
        , oneBadge cfg Badge.dangerLight "dangerLight"
        , oneBadge cfg Badge.successLight "successLight"
        , oneBadge cfg Badge.grayDark "grayDark"
        , oneBadge cfg Badge.primaryDark "primaryDark"
        , oneBadge cfg Badge.warningDark "warningDark"
        , oneBadge cfg Badge.dangerDark "dangerDark"
        , oneBadge cfg Badge.successDark "successDark"
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

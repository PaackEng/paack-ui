module Alerts exposing (stories)

import Element exposing (..)
import Element.Background as Background
import Msg as Msg
import PluginOptions exposing (defaultWithMenu)
import UI.Alert as Alert
import UIExplorer exposing (storiesOf)
import Utils exposing (prettifyElmCode, story)


stories renderConfig =
    storiesOf
        "Alerts"
        [ primaryStory renderConfig
        , successStory renderConfig
        , warningStory renderConfig
        , dangerStory renderConfig
        ]


primaryStory renderConfig =
    story
        ( "Primary"
        , Alert.primary "Hey I just met you"
            |> baseView renderConfig
        , { defaultWithMenu
            | note = prettifyElmCode """
Alert.success "Hey I just met you"
    |> Alert.toEl
"""
          }
        )


successStory renderConfig =
    story
        ( "Success"
        , Alert.success "Hey I just met you"
            |> baseView renderConfig
        , { defaultWithMenu
            | code = prettifyElmCode """
Alert.success "Hey I just met you"
    |> Alert.toEl
"""
          }
        )


warningStory renderConfig =
    story
        ( "Warning"
        , Alert.warning "Hey I just met you"
            |> baseView renderConfig
        , { defaultWithMenu
            | code = prettifyElmCode """
Alert.warning "Hey I just met you"
    |> Alert.toEl
"""
          }
        )


dangerStory renderConfig =
    story
        ( "Danger"
        , Alert.danger "Hey I just met you"
            |> baseView renderConfig
        , { defaultWithMenu
            | code = prettifyElmCode """
Alert.danger "Hey I just met you"
    |> Alert.toEl
"""
          }
        )


baseView renderConfig content =
    Alert.renderElement renderConfig content

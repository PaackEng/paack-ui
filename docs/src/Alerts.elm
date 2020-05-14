module Alerts exposing (stories)

import Element exposing (..)
import Element.Background as Background
import Msg as Msg
import UI.Alert as Alert
import UI.Theme as Theme
import UIExplorer exposing (storiesOf)
import Utils exposing (story)


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
        , { note = """
```elm
Alert.success "Hey I just met you"
    |> Alert.toEl
```
"""
          }
        )


successStory renderConfig =
    story
        ( "Success"
        , Alert.success "Hey I just met you"
            |> baseView renderConfig
        , { note = """
```elm
Alert.success "Hey I just met you"
    |> Alert.toEl
```
"""
          }
        )


warningStory renderConfig =
    story
        ( "Warning"
        , Alert.warning "Hey I just met you"
            |> baseView renderConfig
        , { note = """
```elm
Alert.warning "Hey I just met you"
    |> Alert.toEl
```
"""
          }
        )


dangerStory renderConfig =
    story
        ( "Danger"
        , Alert.danger "Hey I just met you"
            |> baseView renderConfig
        , { note = """
```elm
Alert.danger "Hey I just met you"
    |> Alert.toEl
```
"""
          }
        )


baseView renderConfig content =
    el
        [ width fill
        , height (px 400)
        , Background.color Theme.black
        , alignTop
        ]
        (Alert.toEl renderConfig content)

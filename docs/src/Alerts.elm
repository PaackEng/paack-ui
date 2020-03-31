module Alerts exposing (stories)

import Element exposing (..)
import Element.Background as Background
import Msg as Msg
import UI.Alert as Alert
import UI.Theme as Theme
import UIExplorer exposing (storiesOf)
import Utils exposing (story)


stories =
    storiesOf
        "Alerts"
        [ infoStory
        , successStory
        , warningStory
        , errorStory
        , subtitleStory
        , withCloseButtonStory
        ]


infoStory =
    story
        ( "Info"
        , Alert.info
            |> Alert.withTitle "Hey I just met you"
            |> baseView
        , { note = """
```elm
Alert.info
    |> Alert.withTitle "Hey I just met you"
    |> Alert.toEl
```
"""
          }
        )


successStory =
    story
        ( "Success"
        , Alert.success
            |> Alert.withTitle "Hey I just met you"
            |> baseView
        , { note = """
```elm
Alert.success
    |> Alert.withTitle "Hey I just met you"
    |> Alert.toEl
```
"""
          }
        )


warningStory =
    story
        ( "Warning"
        , Alert.warning
            |> Alert.withTitle "Hey I just met you"
            |> baseView
        , { note = """
```elm
Alert.warning
    |> Alert.withTitle "Hey I just met you"
    |> Alert.toEl
```
"""
          }
        )


errorStory =
    story
        ( "Error"
        , Alert.error
            |> Alert.withTitle "Hey I just met you"
            |> baseView
        , { note = """
```elm
Alert.error
    |> Alert.withTitle "Hey I just met you"
    |> Alert.toEl
```
"""
          }
        )


subtitleStory =
    story
        ( "Subtitle"
        , Alert.info
            |> Alert.withTitle "Hey I just met you"
            |> Alert.withSubtitle "And this is crazy"
            |> baseView
        , { note = """
```elm
Alert.info
    |> Alert.withTitle "Hey I just met you"
    |> Alert.withSubtitle "And this is crazy"
    |> Alert.toEl
```
"""
          }
        )


withCloseButtonStory =
    story
        ( "With Close Button"
        , Alert.info
            |> Alert.withTitle "Hey I just met you"
            |> Alert.withSubtitle "And look, we have a button"
            |> Alert.withCloseButton Msg.NoOp
            |> baseView
        , { note = """
```elm
Alert.info
    |> Alert.withTitle "Hey I just met you"
    |> Alert.withSubtitle "And this is crazy"
    |> Alert.withCloseButton YourCoolMsg
    |> Alert.toEl
```
"""
          }
        )


baseView content =
    el
        [ width fill
        , height (px 400)
        , Background.color Theme.black
        , alignTop
        ]
        (Alert.toEl content)

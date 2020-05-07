module Form.TextField exposing (stories)

import Element exposing (..)
import Form.State as FormMsg
import Msg as RootMsg
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyWithModel)


stories cfg =
    storiesOf
        "TextField"
        [ defaultTextFieldStory cfg
        , usernameTextFieldStory cfg
        , passwordTextFieldStory cfg
        , fullWidthStory cfg
        ]


defaultTextFieldStory cfg =
    story
        ( "Default"
        , TextField.forAny
            |> TextField.textField (always RootMsg.NoOp)
                "My cool default"
                "Value"
            |> TextField.withPlaceholder "Enter your info here"
            |> TextField.withLabelVisible True
            |> TextField.toEl cfg
        , { note = """
```elm
type Msg
    = OnTextFieldChanged String
    | ...


-- Default
, TextField.forAny
    |> TextField.textField OnTextFieldChanged
        "My cool input"
        model.value
    |> TextField.withPlaceholder "Enter your info here"
    |> TextField.withLabelVisible True
    |> TextField.toEl renderCfg
```
"""
          }
        )


usernameTextFieldStory cfg =
    story
        ( "Email"
        , TextField.forEmail
            |> TextField.textField (always RootMsg.NoOp)
                "Enter your email"
                "Value"
            |> TextField.withLabelVisible True
            |> TextField.toEl cfg
        , { note = """
```elm
type Msg
    = OnTextFieldChanged String
    | ...


-- Email
TextField.forEmail
    |> TextField.textField OnTextFieldChanged
        "Enter your email"
        model.emailValue
    |> TextField.withLabelVisible true
    |> TextField.toEl renderCfg
```
"""
          }
        )


passwordTextFieldStory cfg =
    story
        ( "Password"
        , TextField.forPassword
            { show = False, isCurrent = False }
            |> TextField.textField (always RootMsg.NoOp)
                "Enter your password"
                "Value"
            |> TextField.withLabelVisible True
            |> TextField.toEl cfg
        , { note = """
```elm
type Msg
    = OnTextFieldChanged String
    | ...


-- Password
TextField.forPassword
    { show = False, isCurrent = False }
    |> TextField.textField OnTextFieldChanged
        "Enter your password"
        mode.passwordValue
    |> TextField.withLabelVisible True
    |> TextField.toEl renderCfg
```
"""
          }
        )


fullWidthStory cfg =
    story
        ( "Full Width"
        , TextField.forAny
            |> TextField.textField (always RootMsg.NoOp)
                "My TextField"
                ""
            |> TextField.withLabelVisible True
            |> TextField.withWidth TextField.widthFull
            |> TextField.toEl cfg
        , { note = """
```elm
type Msg
    = OnTextFieldChanged String
    | ...


-- Full width
TextField.forAny
    |> TextField.textField OnTextFieldChanged
        "My TextField"
        model.someFieldValue
    |> TextField.withLabelVisible True
    |> TextField.withWidth TextField.widthFull
    |> TextField.toEl renderCfg
```
"""
          }
        )

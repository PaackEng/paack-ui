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
        , TextField.forSinglelineText
            |> TextField.withInput (always RootMsg.NoOp)
                "My cool default"
                "Value"
            |> TextField.withPlaceholder "Enter your info here"
            |> TextField.withLabelNotHidden True
            |> TextField.toEl cfg
        , { note = """
```elm
type Msg
    = OnTextFieldChanged String
    | ...


-- Default
, TextField.forSinglelineText
    |> TextField.withInput OnTextFieldChanged
        "My cool input"
        model.value
    |> TextField.withPlaceholder "Enter your info here"
    |> TextField.withLabelNotHidden True
    |> TextField.toEl renderCfg
```
"""
          }
        )


usernameTextFieldStory cfg =
    story
        ( "Username"
        , TextField.forUsername
            |> TextField.withInput (always RootMsg.NoOp)
                "Enter your email"
                "Value"
            |> TextField.withLabelNotHidden True
            |> TextField.toEl cfg
        , { note = """
```elm
type Msg
    = OnTextFieldChanged String
    | ...


-- Email
TextField.forEmail
    |> TextField.withInput OnTextFieldChanged
        "Enter your email"
        model.emailValue
    |> TextField.withLabelNotHidden true
    |> TextField.toEl renderCfg
```
"""
          }
        )


passwordTextFieldStory cfg =
    story
        ( "Password"
        , TextField.forCurrentPassword
            |> TextField.withInput (always RootMsg.NoOp)
                "Enter your password"
                "Value"
            |> TextField.withLabelNotHidden True
            |> TextField.toEl cfg
        , { note = """
```elm
type Msg
    = OnTextFieldChanged String
    | ...


-- Password
TextField.forCurrentPassword
    |> TextField.withInput OnTextFieldChanged
        "Enter your password"
        mode.passwordValue
    |> TextField.withLabelNotHidden True
    |> TextField.toEl renderCfg
```
"""
          }
        )


fullWidthStory cfg =
    story
        ( "Full Width"
        , TextField.forSinglelineText
            |> TextField.withInput (always RootMsg.NoOp)
                "My TextField"
                "Some big text"
            |> TextField.withLabelNotHidden True
            |> TextField.withWidth TextField.widthFull
            |> TextField.toEl cfg
        , { note = """
```elm
type Msg
    = OnTextFieldChanged String
    | ...


-- Full width
TextField.forSinglelineText
    |> TextField.withInput OnTextFieldChanged
        "My TextField"
        model.someFieldValue
    |> TextField.withLabelNotHidden True
    |> TextField.withWidth TextField.widthFull
    |> TextField.toEl renderCfg
```
"""
          }
        )

module Form.Input exposing (stories)

import Element exposing (..)
import Form.State as FormMsg
import Msg as RootMsg
import UI.Input as Input
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyWithModel)


stories =
    storiesOf
        "Input"
        [ defaultInputStory
        , textStory
        , emailInputStory
        , passwordInputStory
        , placeholderStory
        , fullWidthStory
        ]


defaultInputStory =
    story
        ( "Default"
        , Input.default (always RootMsg.NoOp)
            |> Input.withLabel "My cool default"
            |> Input.toEl
        , { note = """
```elm
type Msg
    = OnInputChanged String
    | ...


-- Default
Input.default OnInputChanged
    |> Input.withLabel "My cool input"
    |> Input.toEl
```
"""
          }
        )


emailInputStory =
    story
        ( "Email"
        , Input.email (always RootMsg.NoOp)
            |> Input.withLabel "Enter your email"
            |> Input.toEl
        , { note = """
```elm
type Msg
    = OnInputChanged String
    | ...


-- Email
Input.email OnInputChanged
    |> Input.withLabel "Enter your email"
    |> Input.toEl
```
"""
          }
        )


passwordInputStory =
    story
        ( "Password"
        , Input.password (always RootMsg.NoOp)
            |> Input.withLabel "Enter your password"
            |> Input.toEl
        , { note = """
```elm
type Msg
    = OnInputChanged String
    | ...


-- Password
Input.password OnInputChanged
    |> Input.withLabel "Enter your password"
    |> Input.toEl
```
"""
          }
        )


textStory =
    story
        ( "Text"
        , Input.default (always RootMsg.NoOp)
            |> Input.withLabel "My Input"
            |> Input.withText "This is the current text"
            |> Input.toEl
        , { note = """
```elm
type Msg
    = OnInputChanged String
    | ...


-- Text
Input.default OnInputChanged
    |> Input.withLabel "My Input"
    |> Input.withText "This is the current text"
    |> Input.toEl
```
"""
          }
        )


placeholderStory =
    storyWithModel
        ( "Placeholder"
        , \model ->
            Input.default (\str -> RootMsg.FormStoriesMsg (FormMsg.OnPlaceholderStoryTyped str))
                |> Input.withLabel "My Input"
                |> Input.withPlaceholder "Enter your info here"
                |> Input.withText model.formStories.placeholderStory
                |> Input.toEl
        , { note = """
```elm
type Msg
    = OnInputChanged String
    | ...


-- Placeholder
Input.default OnInputChanged
    |> Input.withLabel "My Input"
    |> Input.withPlaceholder "Enter your info here"
    |> Input.toEl
```
"""
          }
        )


fullWidthStory =
    story
        ( "Full Width"
        , Input.default (always RootMsg.NoOp)
            |> Input.withLabel "My Input"
            |> Input.withFullWidth
            |> Input.toEl
        , { note = """
```elm
type Msg
    = OnInputChanged String
    | ...


-- Full width
Input.default OnInputChanged
    |> Input.withLabel "My Input"
    |> Input.withFullWidth
    |> Input.toEl
```
"""
          }
        )

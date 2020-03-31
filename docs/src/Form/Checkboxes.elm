module Form.Checkboxes exposing (stories)

import Element exposing (..)
import Form.State as FormMsg
import Msg as RootMsg
import UI.Input.Checkbox as Checkbox
import UIExplorer exposing (storiesOf)
import Utils exposing (storyWithModel)


stories =
    storiesOf
        "Checkbox"
        [ defaultStory ]


defaultStory =
    storyWithModel
        ( "Default"
        , \model -> view model.formStories.checkboxStory
        , { note = """
```elm
type Msg
    = OnCheckboxChanged Bool
    | ...

type alias Model =
    { isMyBoolChecked : Bool }

Checkbox.checkbox OnCheckboxChanged
    |> Checkbox.withIsChecked isMyBoolChecked
    |> Checkbox.withLabel "My cool checkbox"
    |> Checkbox.toEl

```
"""
          }
        )


view : Bool -> Element RootMsg.Msg
view isChecked =
    let
        textVal =
            if isChecked then
                "Checked"

            else
                "Not Checked"
    in
    row [ spacing 10, width fill ]
        [ Checkbox.checkbox (RootMsg.FormStoriesMsg << FormMsg.OnCheckboxToggled)
            |> Checkbox.withIsChecked isChecked
            |> Checkbox.withLabel "Click here"
            |> Checkbox.toEl
        , text textVal
        ]

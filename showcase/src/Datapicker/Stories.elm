module Datapicker.Stories exposing (..)

import UI.Internal.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerUI)


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Datepicker"
        []

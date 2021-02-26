module UI.Effect exposing (Effect(..), perform)

import Task


type Effect msg
    = MsgToCmd msg
    | None


perform : Effect msg -> Cmd msg
perform effect =
    case effect of
        MsgToCmd msg ->
            Task.perform identity <| Task.succeed msg

        None ->
            Cmd.none

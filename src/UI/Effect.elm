module UI.Effect exposing
    ( Effect
    , SideEffect(..)
    , analytics
    , batch
    , msgToCmd
    , none
    , perform
    )

import Task
import UI.Analytics exposing (Analytics)


type alias Effect msg =
    List (SideEffect msg)


type SideEffect msg
    = MsgToCmd msg
    | Analytics Analytics


none : Effect msg
none =
    []


batch : List (Effect msg) -> Effect msg
batch =
    List.concat


msgToCmd : msg -> Effect msg
msgToCmd msg =
    [ MsgToCmd msg ]


analytics : Analytics -> Effect msg
analytics analytics_ =
    [ Analytics analytics_ ]


perform : Effect msg -> Cmd msg
perform effect =
    effect
        |> List.map performSideEffect
        |> Cmd.batch


performSideEffect : SideEffect msg -> Cmd msg
performSideEffect effect =
    case effect of
        MsgToCmd msg ->
            Task.perform identity <| Task.succeed msg

        Analytics _ ->
            Cmd.none

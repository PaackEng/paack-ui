module UI.Effects exposing
    ( Effects, SideEffect(..), none, batch, fromCmd, msgToCmd, analytics, domFocus
    , map, perform
    )

{-| `UI.Effect` is a combination of every command required for `paack-ui` to work correctly.
The data describing each side-effect is publicly available, performing the side-effects is left to the user.

Can be used along with [`elm-program-test`](https://elm-program-test.netlify.app/cmds.html#defining-an-effect-type) to test your application flow.

This module parallels [elm/core's `Platform.Cmd` module](https://package.elm-lang.org/packages/elm/core/1.0.5/Platform-Cmd),
but with some additional helpers for creating `Effect` values.

Note: If you don't know about commands yet, do not worry if this seems confusing at first,
commands will make more sense if you go through the Elm Architecture Tutorial first to see how
they fit in real applications.


# Create

@docs Effects, SideEffect, none, batch, msgToCmd, analytics, domFocus


# Transform

@docs map, perform

-}

import Browser.Dom as Dom
import Task
import UI.Analytics exposing (Analytics)


{-| A list of side-effects to be performed later.
-}
type alias Effects msg =
    List (SideEffect msg)


{-| The `SideEffect msg` type is used for describing commands for later inspection.
-}
type SideEffect msg
    = MsgToCmd msg
    | DomFocus (Result Dom.Error () -> msg) String
    | Analytics Analytics
    | Cmd (Cmd msg)


{-| Tells the `perform` function that there are no side-effects. Parallels `Cmd.none`.
-}
none : Effects msg
none =
    []


{-| Batch effects together. Parallels `Cmd.batch`.
-}
batch : List (Effects msg) -> Effects msg
batch =
    List.concat


{-| Transform the messages produced by a effect. Parallels `Cmd.map`.
-}
map : (a -> b) -> Effects a -> Effects b
map =
    let
        mapSideEffect f e =
            case e of
                MsgToCmd msg ->
                    MsgToCmd (f msg)

                Analytics data ->
                    Analytics data

                DomFocus msg id ->
                    DomFocus (msg >> f) id

                Cmd cmd ->
                    Cmd (Cmd.map f cmd)
    in
    mapSideEffect >> List.map


{-| The effect of returning a `msg`.
-}
msgToCmd : msg -> Effects msg
msgToCmd msg =
    [ MsgToCmd msg ]


{-| The effect of returning `Analytics`.
-}
analytics : Analytics -> Effects msg
analytics analytics_ =
    [ Analytics analytics_ ]


{-| The dom focus effect constructor.
-}
domFocus : (Result Dom.Error () -> msg) -> String -> Effects msg
domFocus msg id =
    [ DomFocus msg id ]

fromCmd : Cmd msg -> Effects msg
fromCmd msg =
    [Cmd msg]

{-| Perform a minimal interpretation of side-effects into commands.
Use this if you don't care to change how to interpret them.
-}
perform : Effects msg -> Cmd msg
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

        DomFocus msg id ->
            Task.attempt msg (Dom.focus id)

        Cmd cmd ->
            Cmd.none

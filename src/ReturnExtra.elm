module ReturnExtra exposing (withCmd, withCmds, withNoCmd)

import Return exposing (Return)


withNoCmd : model -> Return msg model
withNoCmd model =
    ( model, Cmd.none )


withCmd : Cmd msg -> model -> Return msg model
withCmd cmd model =
    ( model, cmd )


withCmds : List (Cmd msg) -> model -> Return msg model
withCmds cmds model =
    ( model, Cmd.batch cmds )

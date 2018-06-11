module RailwayUtils

    open Chessie.ErrorHandling
    open Godot

    let map singleTrackFunction =
        bind (singleTrackFunction >> ok)

    let tee f x =
        f x
        |> ignore
        x

    let log twoTrackInput =
        let failure msgs =
            let message = (String.concat "" msgs)
            GD.Print ("LOG: " + message)
        failureTee failure twoTrackInput

    let logErr twoTrackInput =
        let failure msgs =
            let message = (String.concat "" msgs)
            GD.Print ("ERROR: " + message)
        failureTee failure twoTrackInput

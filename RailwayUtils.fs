module RailwayUtils

    open Chessie.ErrorHandling
    open Godot

    let map singleTrackFunction =
        bind (singleTrackFunction >> ok)

    let tee f x =
        f x |> ignore
        x

    let switch f x =
        f x |> Ok

    let log twoTrackInput =
        let failure msgs = GD.Print (String.concat "ERROR. %A" msgs)
        failureTee failure twoTrackInput

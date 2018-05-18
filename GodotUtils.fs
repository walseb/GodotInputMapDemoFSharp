module GodotUtils

open Godot

type Node with
    member this.getNode<'a when 'a :> Node> (path:string) =
        lazy((this.GetNode(new NodePath(path))) :?> 'a)

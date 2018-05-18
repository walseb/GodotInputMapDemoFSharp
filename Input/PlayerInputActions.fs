namespace InputManagement.PlayerInputActions

open Microsoft.FSharp.Reflection

module PlayerInputActions =

    type MovementActions =
        | MoveUp
        | MoveDown
        | MoveLeft
        | MoveRight
        | Jump

    type AggressiveActions =
        | Attack

    let getUnionCaseNames<'t> =
        FSharpType.GetUnionCases typeof<'t> |> Array.map (fun info -> info.Name) |> Array.toList

    let allActionNames : string array =
        // Merge the two lists into one
        getUnionCaseNames<MovementActions> @ getUnionCaseNames<AggressiveActions>
        |> List.toArray

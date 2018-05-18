namespace InputMapManager

open Godot

open InputManagement.PlayerInputActions

open Chessie.ErrorHandling
open RailwayUtils
open GodotUtils

module configManagement =
    let configPath = "user://input.cfg"

    let HandleLoadError (error : Error) =
        match error with
        | Error.Ok -> ok ignore
        | Error.FileAlreadyInUse -> fail "File already in use"
        | Error.FileNoPermission -> fail "No permission to write"
        | _ -> fail "Can't write config"

    let LoadConfigFromInputMap (config : ConfigFile)=
        HandleLoadError (config.Load(configPath))

    let WriteConfigToFileSystem (config : ConfigFile) =
        HandleLoadError (config.Save(configPath))

    let clearDuplicateInputMapEvents actionName =
        let removeEvent actionName (eventToRemove : InputEvent)  =
            InputMap.ActionEraseEvent(actionName, eventToRemove)

        InputMap.GetActionList(actionName)
        |> Array.iter (fun currentEvent -> if currentEvent :? InputEventKey then removeEvent actionName (currentEvent :?> InputEventKey))

    let addEventToInputMap actionName event =
        InputMap.ActionAddEvent(actionName, event)

    let getDefaultConfig ( config: ConfigFile) : ConfigFile=
        let addActionNameToConfig (config : ConfigFile) actionName controllerLayerIndex =
            let inputEvent = InputMap.GetActionList(actionName).[controllerLayerIndex]
            if inputEvent :? InputEventKey then
                let scancode = OS.GetScancodeString((inputEvent :?> InputEventKey).Scancode)
                config.SetValue("input", actionName, scancode)

        let config = new ConfigFile()

        PlayerInputActions.allActionNames
        |> Array.iter (fun actionName -> addActionNameToConfig config actionName 0)
        config

    let copyConfigToInputMap (config : ConfigFile) : ConfigFile =
        let addActionToInputMap (config : ConfigFile) actionName =
            let createEvent scancode =
                let newEvent = new InputEventKey();
                newEvent.Scancode <- scancode
                newEvent

            let getScancodeIfActionExists (config : ConfigFile) actionName =
                let scancodeString = config.GetValue("input", actionName) :?> string

                match scancodeString with
                | null -> fail ("No such action in config: " + actionName)
                | _ -> OS.FindScancodeFromString(scancodeString) |> ok

            clearDuplicateInputMapEvents actionName

            getScancodeIfActionExists config actionName
            |> map createEvent
            |> map (addEventToInputMap actionName)

        PlayerInputActions.allActionNames
        |> Array.iter (fun string -> addActionToInputMap config string |> log |> ignore)
        config

// Type because needs to be called when starting game without entering key rebind menu
type ConfigManager() =
    static member LoadOrCreateConfig : unit =
        let load (config : ConfigFile) =
            config
            |> configManagement.copyConfigToInputMap
            |> ignore

        let create (config : ConfigFile) =
            config
            |> configManagement.getDefaultConfig
            |> configManagement.copyConfigToInputMap
            |> configManagement.WriteConfigToFileSystem
            |> ignore

        let config = new ConfigFile()

        match config.Load(configManagement.configPath) with
        | Error.Ok ->
            load config
        | _ ->
            create config

type RebindMenu() as this =
    inherit Node()

    let mutable action = ""
    let label = this.getNode ("contextual_help")

    let getButton action =
        this.GetNode(new NodePath("bindings")).GetNode(new NodePath(action)).GetNode(new NodePath("Button")) :?> Button

    member this.WaitForInput (actionKey : string)=
        action <- actionKey

        (label.Force() : Label).Text <- ("Press a key to assign to the '" + actionKey + "' action.")

        this.SetProcessInput true

    override this._Input(inputEvent : InputEvent) =
        let updateKey (inputEventKey : InputEventKey) =
            let updateConfigWithCurrentMaps (config : ConfigFile) =
                configManagement.LoadConfigFromInputMap config |> log |> ignore
                config

            let setValueInConfig action scancode (config : ConfigFile) : ConfigFile =
                config.SetValue("input", action, scancode)
                config

            let scancode = OS.GetScancodeString(inputEventKey.Scancode)

            (getButton action).Text <- scancode

            configManagement.clearDuplicateInputMapEvents action
            configManagement.addEventToInputMap action inputEvent

            new ConfigFile()
            |> updateConfigWithCurrentMaps
            |> setValueInConfig action scancode
            |> configManagement.WriteConfigToFileSystem

        // If false, Input is not a key and is ignored
        match inputEvent :? InputEventKey with
        | true ->
            this.GetTree().SetInputAsHandled();
            this.SetProcessInput false

            (label.Force() : Label).Text <- "Click a key binding to reassign it, or press the Cancel action."

            match inputEvent.IsAction "ui_cancel" with
            | true ->
                ()
            | false ->
                updateKey(inputEvent :?> InputEventKey) |> log |> ignore
        | false -> ()

    override this._Ready() =
        ConfigManager.LoadOrCreateConfig

        let updateButton controllerLayerIndex actionName =
            let inputEvent = InputMap.GetActionList(actionName).[controllerLayerIndex]

            match inputEvent :? InputEventKey with
            | true ->
                let button = getButton actionName
                button.Text <- OS.GetScancodeString((inputEvent :?> InputEventKey).Scancode)
                button.Connect("pressed", this, "WaitForInput", [|actionName|]) |> ignore
            | false ->
                GD.Print("WARNING: Key in default input map isn't a key")

        PlayerInputActions.allActionNames
        |> Array.iter (fun (actionName : string) -> (updateButton 0 actionName))

        this.SetProcessInput false

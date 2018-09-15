namespace InputManager

open Chessie.ErrorHandling
open RailwayUtils
open Godot
open GodotUtils

module PlayerInputActions =
    let moveUp = "MoveUp"
    let moveDown = "MoveDown"
    let moveLeft = "MoveLeft"
    let moveRight = "MoveRight"
    let pickup = "Pickup"
    let sprint = "Sprint"
    let attack = "Attack"
    let aim = "Aim"

    let allActionNames : string array =
        [|
            moveUp
            moveDown
            moveLeft
            moveRight
            pickup
            sprint
            attack
            aim
        |]

module ConfigManagement =
    let inputEventTee (inputEvent : InputEvent) keyAction mouseButtonAction =
        match inputEvent :? InputEventKey with
            | true ->
                keyAction()
                |> ok 
            | false ->
                match inputEvent :? InputEventMouseButton with
                    | true ->
                        mouseButtonAction()
                        |> ok 
                    | false ->
                        fail "Input is not bindable"

    module ScancodeHandling =
        let ScancodeToReadable (inputEvent : InputEvent) scancode =
            let ButtonIndexToReadable (buttonIndex : int) =
                match buttonIndex with
                    | 1 -> "Left Mouse Button"
                    | 2 -> "Right Mouse Button"
                    | 3 -> "Middle Mouse Button"
                    // Wheel
                    | 4 -> "Mouse wheel up"
                    | 5 -> "Mouse wheel down"
                    | 6 -> "Mouse wheel left"
                    | 7 -> "Mouse wheel right"
                    | _ -> "Unknown mouse key"

            inputEventTee inputEvent ( fun _ -> (OS.GetScancodeString scancode)) ( fun _ -> (ButtonIndexToReadable scancode))

        let InputEventToScancode (inputEvent : InputEvent) =
            inputEventTee inputEvent ( fun _ -> ((inputEvent :?> InputEventKey).Scancode)) (fun _ -> ((inputEvent :?> InputEventMouseButton).GetButtonIndex()))

        let inputEventToReadable (inputEvent : InputEvent) =
            inputEvent
            |> InputEventToScancode
            |> bind (ScancodeToReadable inputEvent)

    let ConfigFileToInputMap (config : ConfigFile) =
        let RemoveInputEventsWithAction actionName =
            let removeEvent actionName (eventToRemove : InputEvent)  =
                InputMap.ActionEraseEvent(actionName, eventToRemove)

            InputMap.GetActionList(actionName)
            |> Seq.iter (fun currentEvent -> if currentEvent :? InputEventKey then removeEvent actionName (currentEvent :?> InputEventKey))

            InputMap.GetActionList(actionName)
            |> Seq.iter (fun currentEvent -> if currentEvent :? InputEventMouseButton then removeEvent actionName (currentEvent :?> InputEventMouseButton))

        let addAction(config : ConfigFile) actionName =
            let getInputEventFromConfig (config : ConfigFile) actionName =
                let createInputEventKey scancode =
                    let newEvent = new InputEventKey();
                    newEvent.Scancode <- scancode
                    newEvent

                let createInputEventMouseButton scancode =
                    let newEvent = new InputEventMouseButton();
                    newEvent.ButtonIndex <- scancode
                    newEvent

                let getScancodeFromConfig actionName =
                    let value = config.GetValue("input", actionName)
                    match value :? int with
                    | true ->
                        ok (value :?> int)
                    | false ->
                        fail "Config corrupted"

                let scancodeToInputEvent scancode =
                    if scancode > 7
                    then (createInputEventKey scancode :> InputEvent)
                    else (createInputEventMouseButton scancode :> InputEvent)

                getScancodeFromConfig actionName
                |> map scancodeToInputEvent

            let addEventToInputMap actionName (actionEvent : InputEvent) =
                InputMap.ActionAddEvent(actionName, actionEvent)

            getInputEventFromConfig config actionName
            |> map (addEventToInputMap actionName)

        let mutable failText = ""
        PlayerInputActions.allActionNames
        // First remove all events with action, then add action event to action
        |> Array.iter (fun action ->
                       RemoveInputEventsWithAction action
                       addAction config action
                       // If fail
                       |> failureTee (fun text -> (failText <- (String.concat "" text)))
                       |> logErr
                       |> ignore)

        match failText with
            | "" -> ok config
            | _ -> fail failText

    module ConfigFileIO =
        let configPath = "user://input.cfg"

        let HandleIOError (error : Error) =
            match error with
            | Error.Ok ->
                ok ()
            | Error.FileAlreadyInUse -> fail "File already in use"
            | Error.FileNoPermission -> fail "No permission to write"
            | Error.DoesNotExist -> fail "Config does not exist"
            | _ -> fail "Can't write config"

        let WriteConfigToFileSystem (config : ConfigFile) =
            HandleIOError (config.Save(configPath))
            |> map (fun _ -> config)

        let LoadConfigWithFileSystemConfig() =
            let config = new ConfigFile()

            HandleIOError (config.Load(configPath))
            |> map (fun _ -> config)

    let AddKeyToConfig action (inputEvent : InputEvent) (config : ConfigFile)=
        let setValueInConfig action (config : ConfigFile) =
            ScancodeHandling.InputEventToScancode inputEvent
            |> map (fun scancode -> (config.SetValue("input", action, scancode)))
            // We need to return a ConfigFile
            |> map (fun _ -> config)

        setValueInConfig action config
                       
    let GetDefaultConfig ( config: ConfigFile) : ConfigFile=
        let addActionNameToConfig (config : ConfigFile) actionName controllerLayerIndex =
            let inputEvent = (InputMap.GetActionList(actionName).[controllerLayerIndex] :?> InputEvent)
            AddKeyToConfig actionName inputEvent config 
            |> bind ConfigFileIO.WriteConfigToFileSystem

        let config = new ConfigFile()

        // Just in case any input maps have been changed, add those back
        InputMap.LoadFromGlobals()

        PlayerInputActions.allActionNames
        |> Array.iter (fun actionName -> addActionNameToConfig config actionName 0 |> logErr |> ignore)
        config

    let LoadOrCreateConfig() =
        let load (config : ConfigFile) =
            config
            |> ConfigFileToInputMap

        let create (config : ConfigFile) =
            config
            |> GetDefaultConfig
            |> ConfigFileToInputMap
            |> bind ConfigFileIO.WriteConfigToFileSystem

        let config = new ConfigFile()

        match config.Load(ConfigFileIO.configPath) with
        | Error.Ok ->
            match (load config) with
                | Ok(value,[]) ->
                    ()
                | Bad(msgs) ->
                    create config
                    |> logErr
                    |> ignore
        | _ ->
            create config
            |> logErr
            |> ignore

// GUI for handling rebinds
type RebindMenu() as this =
    inherit Node()

    let mutable action = ""
    let label = this.getNode ("contextual_help")

    // Back button
    let _on_BackButton_pressed() : unit =
        this.GetTree().ChangeScene("res://Assets/Scenes/MainMenu/MainMenu.tscn") |> ignore

    let getButton action =
        this.GetNode(new NodePath("bindings")).GetNode(new NodePath(action)).GetNode(new NodePath("Button")) :?> Button

    let startPolling (actionKey : string)=
        action <- actionKey
        (label.Force() : Label).Text <- ("Press a key to assign to the '" + actionKey + "' action.")
        this.SetProcessInput true

    let stopPolling() =
        (label.Force() : Label).Text <- "Click a key binding to reassign it, or press the Cancel action."
        this.SetProcessInput false

    let changeButtonText actionName (inputEvent : InputEvent) =
        let button = getButton actionName
        ConfigManagement.ScancodeHandling.inputEventToReadable inputEvent
        |> map (fun (actionName : string) ->  button.SetText(actionName))
        |> map (fun _ -> button)

    override this._Input(inputEvent : InputEvent) =
        let registerEvent() =
            // A legitimate key has been pressed (I.E a key that isn't mouse movement)
            stopPolling()

            match inputEvent.IsAction "ui_cancel" with
                | true ->
                    ok ()
                | false ->
                    ConfigManagement.ConfigFileIO.LoadConfigWithFileSystemConfig()
                    |> bind (ConfigManagement.AddKeyToConfig action inputEvent)
                    |> bind (ConfigManagement.ConfigFileIO.WriteConfigToFileSystem)
                    |> map (fun _ -> inputEvent)
                    |> bind (changeButtonText action)
                    // Workaround, ok result needs to be nil
                    |> map (fun _ -> ())

        this.GetTree().SetInputAsHandled();

        ConfigManagement.inputEventTee inputEvent registerEvent registerEvent
        |> logErr
        |> ignore

    override this._Ready() =
        stopPolling()

        let updateButton controllerLayerIndex actionName =
            let inputEvent = InputMap.GetActionList(actionName).[controllerLayerIndex] :?> InputEvent

            let setupButton (inputEvent : InputEvent) =
                let handleConnectError (error : Error) =
                    match error with
                    | Error.Ok ->
                        ok ()
                    | Error.DoesNotExist -> fail "Could not find button with action name, object is missing in rebind scene"
                    | Error.LinkFailed -> fail "Connect failed"
                    | _ -> fail "Rebind menu can't connect button"

                inputEvent
                |> changeButtonText actionName
                |> map (fun button ->
                        let actionNameArray : Array = new Array()
                        actionNameArray.Add actionName
                        handleConnectError (button.Connect("pressed", this, "startPolling", actionNameArray))
                        |> logErr
                        |> ignore)

            ConfigManagement.inputEventTee inputEvent (fun _ -> setupButton(inputEvent)) (fun _ -> (setupButton inputEvent))
            |> logErr
            |> ignore

        ConfigManagement.LoadOrCreateConfig()

        PlayerInputActions.allActionNames
        |> Array.iter (fun (actionName : string) -> (updateButton 0 actionName))

        this.SetProcessInput(false)

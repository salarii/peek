interface State
    exposes [
        create,
        resetActiveCommand,
        setCommandOutput,
        getCommandOutput,
        setTerminalState,
        getTerminalState,
        setSystemData,
        getSystemData,
        getCommand,
        setCommand, 
        StateType,
        TerminalLineStateType]
    imports []

TerminalLineStateType : {
    commandHistory : List (List U8),
    historyCnt : I32,
    content : List U8,
    cursor : { row : I32, col : I32 },
    prompt : List U8,
}

SystemDataType : {
    homePath : Str, 
    current : Str,
}

StateType := { history : List Str, lastCommandOut : List Str, activeCmd : Str, terminal : TerminalLineStateType, system : SystemDataType }

create : Str -> StateType
create = \ initialText ->
    init = {
        commandHistory : [],
        historyCnt : -1,
        content : Str.toUtf8 initialText,
        cursor: { row: 1, col: 1},
        prompt : [],
    }
    @StateType  {history : [], lastCommandOut : [], activeCmd : "", terminal : init, system : {homePath : "", current : ""}}

resetActiveCommand : StateType -> StateType
resetActiveCommand = \ @StateType content ->
    @StateType { content & activeCmd : "", lastCommandOut : [] }

setCommand : Str, StateType -> StateType
setCommand = \command, @StateType content ->
    @StateType { content & activeCmd : command } 

setCommandOutput : Str, StateType -> StateType
setCommandOutput = \out, @StateType content ->
    splited = Str.split out "\n"
    @StateType { content &  history : (List.concat content.history splited ), lastCommandOut : splited  } 

setTerminalState : TerminalLineStateType, StateType -> StateType
setTerminalState = \ terminalState, @StateType content ->
    @StateType { content &  terminal : terminalState } 

setSystemData : SystemDataType, StateType -> StateType
setSystemData = \ systemData, @StateType content ->
    prompt =
        systemData.current
        |> Str.concat " $> "
        |> Str.toUtf8

    terminal =  content.terminal
    @StateType { content &  system : systemData, terminal : { terminal & prompt : prompt } } 

getCommandOutput : StateType -> List Str
getCommandOutput = \@StateType content ->
    content.lastCommandOut

getHistoryOutput : StateType -> List Str
getHistoryOutput = \@StateType content ->
    content.history

getCommand : StateType -> Str
getCommand = \@StateType content ->
    content.activeCmd

getTerminalState : StateType -> TerminalLineStateType
getTerminalState = \@StateType content ->
    content.terminal

getSystemData : StateType -> SystemDataType
getSystemData = \ @StateType content ->
    content.system
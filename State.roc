interface State
    exposes [
        create,
        setCommandOutput,
        getCommandOutput,
        setTerminalState,
        getTerminalState,
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
}

StateType := { history : Str, lastCommandOut : Str, activeCmd : Str, terminal : TerminalLineStateType }

create : Str -> StateType
create = \ initialText ->
    init = {
        commandHistory : [],
        historyCnt : -1,
        content : Str.toUtf8 initialText,
        cursor: { row: 1, col: 1},
    }
    @StateType  {history : "", lastCommandOut : "", activeCmd : "", terminal : init}

setCommand : Str, StateType -> StateType
setCommand = \command, @StateType content ->
    @StateType { content & activeCmd : command } 

setCommandOutput : Str, StateType -> StateType
setCommandOutput = \out, @StateType content ->
    @StateType { content &  history : (Str.concat content.history out), lastCommandOut : out } 

setTerminalState : TerminalLineStateType, StateType -> StateType
setTerminalState = \ terminalState, @StateType content ->
     @StateType { content &  terminal : terminalState } 

getCommandOutput : StateType -> Str
getCommandOutput = \@StateType content ->
    content.lastCommandOut

getHistoryOutput : StateType -> Str
getHistoryOutput = \@StateType content ->
    content.history

getCommand : StateType -> Str
getCommand = \@StateType content ->
    content.activeCmd

getTerminalState : StateType -> TerminalLineStateType
getTerminalState = \@StateType content ->
    content.terminal
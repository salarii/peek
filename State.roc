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
        setAppMode,
        getAppMode,
        setFile,
        getFile,
        updatePrompt,
        StateType,
        TerminalLineStateType,
        PatternType,
        ModifiersType,
        CommandType,
        ConfigType,
        AppModeType,
        ]
    imports []

PatternType : [ Regex [Allow Str,Color Str, Blacklist Str], Allow Str, Blacklist Str, Color Str  ]

ModifiersType  : [ NumberLines]

CommandType : [
    Search,
    SearchSection U32 U32 PatternType,
    FromLineToLine  I32 I32,
    FromPatternToPattern PatternType PatternType,
]

ConfigType :
    { command: CommandType, modifiers : Set ModifiersType, patterns : List PatternType  }

SearchSetupType : [ None, Configured ConfigType ]

AppModeType : [System, Search, Quitting]

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

StateType := {
    history : List Str,
    lastCommandOut : List Str,
    file: List Str,
    terminal : TerminalLineStateType,
    system : SystemDataType,  
    config : SearchSetupType,
    mode : AppModeType,
    }

create : Str -> StateType
create = \ initialText ->
    init = {
        commandHistory : [],
        historyCnt : -1,
        content : Str.toUtf8 initialText,
        cursor: { row: 1, col: 1},
        prompt : [],
    }

    @StateType  {
        history : [],
        lastCommandOut : [],
        file : [],
        terminal : init,
        system : {homePath : "", current : ""},
        config : None,
        mode : System,
        }

resetActiveCommand : StateType -> StateType
resetActiveCommand = \ @StateType content ->
    @StateType { content & lastCommandOut : [] }

setFile : StateType, List Str -> StateType
setFile = \ @StateType content, file ->
    @StateType { content & file : file } 

setCommandOutput : StateType, Str -> StateType
setCommandOutput = \ @StateType content, out ->
    splited = Str.split out "\n"
    @StateType { content &  history : (List.concat content.history splited ), lastCommandOut : splited  } 

setTerminalState : StateType, TerminalLineStateType -> StateType
setTerminalState = \ @StateType content, terminalState ->
    @StateType { content &  terminal : terminalState } 

setSystemData : StateType, SystemDataType -> StateType
setSystemData = \ @StateType content, systemData ->
    @StateType { content &  system : systemData }

updatePrompt : StateType -> StateType
updatePrompt = \  @StateType content ->
    terminal =  content.terminal # I would like to place it  directly { content.terminal & prompt : prompt }
    if content.mode == System then
        prompt =
            content.system.current
            |> Str.concat " $> "
            |> Str.toUtf8
        @StateType { content &  terminal : { terminal & prompt : prompt } } 
    else if content.mode == Search then
        @StateType { content &  terminal : { terminal & prompt : [] } } 
    else 
        @StateType content

setAppMode : StateType, AppModeType -> StateType
setAppMode = \@StateType content, mode ->
    @StateType { content & mode : mode }

getCommandOutput : StateType -> List Str
getCommandOutput = \@StateType content ->
    content.lastCommandOut

getHistoryOutput : StateType -> List Str
getHistoryOutput = \@StateType content ->
    content.history

getTerminalState : StateType -> TerminalLineStateType
getTerminalState = \@StateType content ->
    content.terminal

getSystemData : StateType -> SystemDataType
getSystemData = \ @StateType content ->
    content.system

getFile : StateType -> List Str
getFile = \ @StateType content ->
    content.file 

getAppMode : StateType -> AppModeType
getAppMode = \@StateType content ->
    content.mode
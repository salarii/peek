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
        setCommandHistory,
        getCommandHistory,
        setTerminalHistory,
        updatePrompt,
        createConfig,
        StateType,
        TerminalLineStateType,
        PatternType,
        ModifiersType,
        CommandType,
        ConfigType,
        AppModeType,
        SectionType,
        ]
    imports [Regex.{regexMagic,MagicType}]

PatternType : [ Regex [Allow Str,Color Str, Blacklist Str], Allow Str, Blacklist Str, Color Str  ]

ModifiersType : [ NumberLines, LogicAND]

SectionType : {before: Nat, after:  Nat, pattern : PatternType}

CommandType : [
    None,
    Search,
    SearchSection SectionType,
    FromPatternToPattern PatternType PatternType,
    ]

HistoryType : List (List U8)

ConfigType : {
    limit : List [FromLineToLine  I32 I32],
    command: CommandType, 
    modifiers : Set ModifiersType, 
    patterns : List PatternType,
    regexMagic : MagicType, # remember to remove this one day : )
    }

SearchSetupType : [ None, Configured ConfigType ]

AppModeType : [System, Search, Quitting]

TerminalLineStateType : {
    commandHistory : HistoryType,
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
    #history : List Str,
    lastCommandOut : List Str,
    file: List Str,
    terminal : TerminalLineStateType,
    system : SystemDataType,  
    config : SearchSetupType,
    mode : AppModeType,
    commandsHistory : { sys : HistoryType,  search : HistoryType},
    }



createConfig :  List [FromLineToLine  I32 I32], CommandType, Set ModifiersType, List PatternType -> ConfigType
createConfig = \  limit, command, modifiers, patterns -> 
    {limit : limit, command : command, modifiers : modifiers, patterns : patterns, regexMagic : regexMagic,}


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
        #history : [],
        lastCommandOut : [],
        file : [],
        terminal : init,
        system : {homePath : "", current : ""},
        config : None,
        mode : System,
        commandsHistory : { sys : [],  search : []},
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
    #@StateType { content &  history : (List.concat content.history splited ), lastCommandOut : splited  } 
    @StateType { content &  lastCommandOut : splited  }
    
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

#getHistoryOutput : StateType -> List Str
#getHistoryOutput = \@StateType content ->
#    content.history

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
    
setCommandHistory : StateType, { sys : HistoryType,  search : HistoryType} -> StateType
setCommandHistory = \@StateType content, history -> 
    @StateType { content & commandsHistory  : history }
    
getCommandHistory : StateType -> { sys : HistoryType,  search : HistoryType}
getCommandHistory = \@StateType content ->
    content.commandsHistory 
    
setTerminalHistory : StateType, List (List U8) -> StateType
setTerminalHistory = \@StateType content, history -> 
    terminal =  content.terminal 
    @StateType { content & terminal : { terminal & commandHistory : history, historyCnt : -1 } }


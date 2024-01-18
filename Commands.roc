interface Commands
    exposes [
        handleUserCommand,
        quitCommand, 
        replaceTilde, 
        recoverConfigFromInput, 
        runParser,
        configMiniParser,
        updateConfig,
        ParserType,
        MiniParserType,
        ParserOutcomeType,
        MiniParserDataType,
        OperationType
        ]
    imports [
        pf.File,
        pf.Path,
        pf.Task.{ Task },
        Utils,
        Regex,
        SearchText,
        Regex.{ParsingResultType, regexMagic, MagicType},
        State,
        System,
        State.{StateType, PatternType, ModifiersType, CommandType, ConfigType},
        "help" as helpStr : Str,
    ]

quitCommand : Str
quitCommand = "peekQuit" 

SimpleType : Str

OperationType : [
        Regex, 
        Allow, 
        Blacklist, 
        Color, 
        LogicalAnd, ] 

ParserConfigType : { exclusive: Dict OperationType (List OperationType), options : Set OperationType }

MiniParserDataType : [ Simple SimpleType, ParserConfig ParserConfigType, Condition Bool, Other [FromLineToLine  I32 I32, NumberLines, None] ]

ParserOutcomeType : [ Completed MiniParserDataType, Continue MiniParserDataType, Error ]  

MiniParserType : { handlers: Dict Str (ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str), data : MiniParserDataType, left : Str }

ParserPhasesType : [ OpenAnd, EndAnd, AndPattern, AndConfig, Config, Pattern, FullCommands ]

ParserOutType : { config:  Set OperationType, pattern : Str }

ParserDataType : { queue : List ParserPhasesType, content: List ParserOutType }

ParserType : { current : MiniParserType, data: ParserDataType, regexMagic: MagicType }

dummy : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str 
dummy = \ parsed, miniParser -> 
    Ok (Completed miniParser)

insertTag : ParsingResultType, MiniParserDataType, OperationType -> ParserOutcomeType
insertTag = \ _parsed, miniParserData, operation -> 
    when miniParserData is 
        ParserConfig configType -> 
            when Dict.get configType.exclusive operation is 
                Ok exclusiveLst -> 
                   
                    List.walkUntil exclusiveLst Bool.false (\ status, tag -> 
                        if Set.contains configType.options tag == Bool.true then
                            Break Bool.true
                        else 
                            Continue Bool.false    
                    )
                    |> ( \ exclusivePresent ->
                        if exclusivePresent == Bool.true then 
                            Error    
                        else
                            Continue (ParserConfig {configType & options : Set.insert configType.options operation })
                        )
                Err _ -> Continue (ParserConfig {configType & options : Set.insert configType.options operation })
        _ -> Error
    
colorHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
colorHandler = \ parsed, miniParser ->
    Ok (insertTag parsed miniParser Color)

regexHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
regexHandler = \ parsed, miniParser -> 
    Ok (insertTag parsed miniParser Regex)

blackListedHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
blackListedHandler = \ parsed, miniParser -> 
    Ok (insertTag parsed miniParser Blacklist)

logicalANDHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
logicalANDHandler = \ parsed, miniParser -> 
    Ok (insertTag parsed miniParser LogicalAnd)

concludeHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
concludeHandler = \ _parsed, miniParserData -> 
    Ok (Completed miniParserData)

configHandlers : Dict Str (ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str)
configHandlers =
    Dict.empty {}
    |> Dict.insert "^[cC]" colorHandler
    |> Dict.insert "^[rR]" regexHandler
    |> Dict.insert "^(\\^)(\\d+)" dummy
    |> Dict.insert "^(>)(\\d+)" dummy
    |> Dict.insert "^(<)(\\d+)" dummy
    |> Dict.insert "^[bB]" blackListedHandler
    |> Dict.insert "^[a][n][d]" logicalANDHandler
    |> Dict.insert "^@" concludeHandler

fullCommandsHandlers : Dict Str (ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str)
fullCommandsHandlers =
    Dict.empty {}
    |> Dict.insert "^[Nn][Ll]@\\s" createNumberLines
    |> Dict.insert "^(\\d+|s)->(\\d+|e)@\\s" createLineToLine

    #|> Dict.insert "^([Rr])?@(.+)->([Rr])?@(\\S+)\\s" createPatternToPattern

fullCommandMiniParser =  { handlers: fullCommandsHandlers, data :  Other None, left : "" }

createNumberLines : createNumberLines, MiniParserDataType -> Result ParserOutcomeType Str
createNumberLines = \ parsed, miniParser -> 
    Ok (Completed ( Other NumberLines ) )
        
createLineToLine : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
createLineToLine = \ parsResult, _miniParser ->
    when ( 
        Regex.getValue [0] 0 parsResult.captured,
        Regex.getValue [1] 0 parsResult.captured)  is
            (Ok pat1,Ok pat2) ->
                when (pat1, pat2) is 
                    (['s'],['e'])->
                        Ok ( Completed (Other (FromLineToLine  1 -1)) )
                    (['s'],val)->
                        Ok ( Completed (Other (FromLineToLine  1 (Utils.asciiArrayToNumber val Str.toI32)) ) )
                    (val,['e'])->
                        Ok ( Completed (Other (FromLineToLine  (Utils.asciiArrayToNumber val Str.toI32) -1) ) )
                    (valStart,valEnd)->
                        Ok ( Completed (Other (FromLineToLine  (Utils.asciiArrayToNumber valStart Str.toI32)  (Utils.asciiArrayToNumber valEnd Str.toI32)) ) )
                    _ ->
                        Err "Command processing error : \(Utils.utfToStr parsResult.matched) comand"
            _ -> Err "Command processing error : \(Utils.utfToStr parsResult.matched) comand"
    
simpleHandlers =
    Dict.empty {}
    |> Dict.insert "^(\\S+)\\s" simpleHandler

simpleHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
simpleHandler = \ parsed, miniParserData -> 
    when Regex.getValue [0] 0 parsed.captured is 
        Ok pattern ->
            Ok ( Completed (Simple ( Utils.utfToStr pattern )) )
        _-> Err "error duing command parsing " 

closeANDHandlers =
    Dict.empty {}
    |> Dict.insert "^\\)" conditionHit  

configData =
    {
        exclusive: 
                Dict.empty {}
                |> Dict.insert  LogicalAnd [Regex, Color]
                |> Dict.insert  Blacklist [ Color]
                |> Dict.insert  Color [LogicalAnd, Blacklist]
                |> Dict.insert  Regex [LogicalAnd],
        options : Set.empty {}
    }

configMiniParser =  { handlers: configHandlers, data :  ParserConfig configData, left : "" }

conditionHit : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
conditionHit = \ parsed, data -> 
    Ok (Completed ( Condition parsed.matchFound ) )

evaluate : MiniParserDataType, ParserType -> Result ParserType Str
evaluate = \ data, parser ->
    updatedQueue = 
        parser.data.queue
        |> List.dropFirst 1
    when data is
        Other other ->
            when other is
                None -> 
                    Ok
                        { parser &
                            current : configMiniParser,
                            data : { queue : updatedQueue |> List.prepend Config, content : parser.data.content } }

                (FromLineToLine  _ _) | NumberLines ->
                    Ok
                        { parser &
                            current : fullCommandMiniParser,
                            data : { queue : updatedQueue |> List.prepend FullCommands, content : parser.data.content } }
                _ -> Err  "unknown problem with command processing" 
        Simple simpleData ->
            when parser.data.queue is 
                [AndPattern, ..] ->
                    when parser.data.content is
                        [] ->  
                            Err  "unknown problem with command processing" 
                        [.. as head, last] ->
                            elems = List.append head { last & pattern : simpleData }
                            Ok
                                { parser &
                                    current : { handlers: closeANDHandlers, data : Condition Bool.false, left : "" },
                                    data : { queue : updatedQueue |> List.prepend EndAnd, content : elems } }
                [Pattern,..] ->
                    when parser.data.content is
                        [] ->  
                            Err  "unknown problem with command processing" 
                        [.. as head, last] ->
                            elems = List.append head { last & pattern : simpleData }
                            Ok
                                { parser &
                                    current : configMiniParser,
                                    data : { queue : updatedQueue |> List.prepend Config, content : elems } }
                _ -> 
                    Err "not implemented" 
        ParserConfig configDataProcessed ->                
            if Set.isEmpty configDataProcessed.options then
                when parser.data.queue is 
                    [AndConfig, ..] -> 
                        Ok  
                            { parser &
                                current : { handlers: simpleHandlers, data : Simple  "", left : "" },
                                data : { queue : (updatedQueue |> List.prepend AndPattern), content : parser.data.content} } 
                    [Config, ..] ->
                        content = List.append  parser.data.content { config: Set.empty {}, pattern : "" }
                        Ok
                            { parser &
                                current : { handlers: simpleHandlers, data : Simple  "", left : "" },
                                data : { queue : (updatedQueue |> List.prepend Pattern), content : content} } 
                    _ -> 
                        Err "unknown problem with command processing"    
            else
                #  remember to fix queue
                updateParser : ParserType, OperationType -> ParserType
                updateParser = \ inParser, operation ->
                    when inParser.data.content  is 
                    [] ->  
                        inParser 
                    [.. as head, last] ->
                        elem = { config: last.config |> Set.insert operation, pattern : "" }  
                        { inParser &
                            data : { queue : inParser.data.queue, content : (List.append head elem)} }
        
                (if Set.contains configDataProcessed.options LogicalAnd == Bool.true then

                    entryHandlers =
                        Dict.empty {}
                        |> Dict.insert "^\\("  conditionHit
                    { parser &
                            current : { handlers: entryHandlers, data : Condition Bool.false, left : "" },
                            data : { queue : [OpenAnd], content : parser.data.content } }
                else
                    content = List.append  parser.data.content { config: Set.empty {}, pattern : "" }
                    { parser &
                        current : { handlers: simpleHandlers, data : Simple  "", left : "" },
                        data : { queue : (updatedQueue |> List.prepend Pattern), content : content} } 
                )
                |> ( \ inProcessParser -> 
                    if Set.contains configDataProcessed.options Regex == Bool.true then 
                        (updateParser inProcessParser Regex)
                    else 
                        inProcessParser )
                |> ( \ inProcessParser -> 
                    if Set.contains configDataProcessed.options Blacklist == Bool.true then 
                        (updateParser inProcessParser Blacklist)
                    else 
                        inProcessParser )
                |> ( \ inProcessParser -> 
                    if Set.contains configDataProcessed.options Color == Bool.true then 
                        (updateParser inProcessParser Color)
                    else 
                        inProcessParser )
                |> ( \ inProcessParser -> Ok inProcessParser)
        Condition cond -> 
            when parser.data.queue is 
                [OpenAnd, ..] ->
                    Ok 
                        { parser &
                            current : { handlers: closeANDHandlers, data : Condition Bool.false, left : "" },
                            data : { queue : updatedQueue |> List.prepend EndAnd, content : parser.data.content } }
                
                [EndAnd, .. as tail] -> 
                    if cond == Bool.true then 
                        if List.isEmpty parser.data.content then 
                            Err "error in and command, empty pattern section"
                        else
                            when tail is 
                                [] ->
                                    Ok {parser & data : { queue : [], content : parser.data.content }  }   
                                _ -> 
                                    evaluate data  {parser & data : { queue : [], content : parser.data.content }  }
                    else 
                        andHandlers =
                            Dict.empty {}
                            |> Dict.insert "^[rR]" regexHandler
                            |> Dict.insert "^[bB]" blackListedHandler
                            |> Dict.insert "^@" concludeHandler
                            
                        andConfigData = { exclusive: Dict.empty {}, options : Set.empty {} }
                        content = List.append parser.data.content { config: Set.empty {}, pattern : "" }
                        Ok { parser &
                            current : { handlers: andHandlers, data : ParserConfig andConfigData, left : "" },
                            data : { queue : updatedQueue |> List.prepend AndConfig, content : content } }
                _ -> 
                    Err "unknown problem with command processing"

isParserCompleted : ParserDataType -> Bool
isParserCompleted = \ parserData -> 
    List.isEmpty parserData.queue

runParser : Str, ParserType -> Result ParserDataType Str
runParser = \ input, parser ->
    loopCommand : Str, MiniParserType -> Result MiniParserType Str
    loopCommand = \ feedRaw, miniParser ->
        feed = Str.trimStart feedRaw
        if Str.isEmpty feed then
            # if isCompleted miniParser.data then
            #    Ok miniParser                
            # else
                Err "error while processing command"
        else
            Dict.keys miniParser.handlers
            |> List.walkUntil (Ok miniParser) (\ state, pattern ->
                when state  is 
                    Ok config ->
                        when Regex.parseStrMagic feed pattern parser.regexMagic is 
                            Ok parsed ->
                                if parsed.matchFound == Bool.true then
                                    when Dict.get config.handlers pattern is
                                        Ok handler ->
                                            dataResult = handler parsed config.data
                                            when dataResult is
                                                Ok data -> 
                                                    when data is 
                                                        Continue parsData ->
                                                            Break (loopCommand (Utils.utfToStr parsed.left) {config & data : parsData })  
                                                        Completed parsData ->
                                                            Break ( Ok {config & data : parsData, left : Utils.utfToStr parsed.left })
                                                        Error -> Continue ( Ok {miniParser & left : input})
                                                Err _ -> Break (Err "error while processing \(input)")
                                        Err _ -> 
                                            Break (Err "internal application error, during comand \(input) analysis")
                                else 
                                    Continue (Ok {config & left : input})
                            Err message -> Break (Err message)
                    Err message -> Break (Err message)
                    )
    loopCommand  input parser.current
    |> ( \ parsedResult ->
        when parsedResult is 
            Ok  parsed -> 
                when evaluate parsed.data parser  is
                    Ok alteredParser ->
                        if #isParserCompleted alteredParser.data || 
                            Str.isEmpty (Str.trimStart parsed.left) then
                            # validate if parsing correct
                            Ok alteredParser.data
                        else 
                            runParser parsed.left alteredParser
                    Err message -> Err message  

            Err message -> Err message  )                
    
updateConfig : ConfigType, List ParserOutType -> Result ConfigType Str
updateConfig = \ config, parserDataLst ->
    if List.isEmpty parserDataLst == Bool.true then
        Ok config
    else 
        modifHandlers = 
            Dict.empty {}
            |> Dict.insert Color colorUpdate
            |> Dict.insert Regex regexUpdate
            |> Dict.insert Blacklist blacklistUpdate
                    
        goOverOptions: List OperationType, PatternType -> Result PatternType Str
        goOverOptions = \ operationLst, pattern ->            
            List.walkTry operationLst pattern ( \ state, type ->
                when Dict.get  modifHandlers type is 
                    Ok handler ->
                        handler state
                    Err _ -> Err "unknown problem during command processing" )

        # bit ugly (state, overall apperance)
        ( List.walkUntil parserDataLst (parserDataLst, Err "missing command data") (\ state, data ->
            if Set.contains data.config LogicalAnd then
                if data.pattern == "Stop" then
                    Break ( List.dropFirst state.0 1, state.1 )
                else 
                    (goOverOptions  
                        (Set.toList (Set.remove data.config LogicalAnd) )
                        ( Allow (LogicalAnd []) ) )
                    |> (\ result -> 
                        Continue ( List.dropFirst state.0 1, result ) )
            else  
                if Str.isEmpty data.pattern then
                    Break (state.0, Err "unknown problem during command processing" )
                else 
                    curPattern = 
                        when state.1 is 
                            Ok pattern -> 
                                pattern
                            Err _ -> Allow (Plain data.pattern)
                    when goOverOptions (Set.toList data.config) curPattern is
                                Ok  updatedPattern ->
                                    when updatedPattern is  
                                        Allow (LogicalAnd _) | Blacklist (LogicalAnd _)  -> 
                                            Continue (List.dropFirst state.0 1, (Ok updatedPattern))
                                        _ -> Break (List.dropFirst state.0 1, (Ok updatedPattern))
                                Err message -> Break (List.dropFirst state.0 1, (Err message)) 
                    ))
        |> (\ patternResult ->
            when patternResult.1 is
                Ok pattern -> 
                    (if config.command == None then
                        {config & command : Search, patterns : List.append config.patterns pattern}
                    else
                        {config & patterns : List.append config.patterns pattern})
                    |> updateConfig patternResult.0
                Err message -> Err message
        )          

# mergeConfigs : ConfigType, ConfigType  -> ConfigType
# mergeConfigs = \configDest, newConfig ->
#     updateCommand  = 
#         when (configDest.command, newConfig.command ) is 
#             (_,None) -> configDest.command
#             (None, _ ) | (Search,_)-> newConfig.command
#             (SearchSection sectionDest, SearchSection sectionNew) -> 
#                 SearchSection (List.concat sectionDest sectionNew)
#             _ -> configDest.command
            
#     mergedlimit = List.concat  configDest.limit newConfig.limit   

#     mergedmodifiers = Set.union configDest.modifiers newConfig.modifiers
#     mergedPatterns = List.concat  configDest.patterns newConfig.patterns

#     {
#         configDest  & 
#             command : updateCommand,
#             limit : mergedlimit, 
#             patterns: mergedPatterns, 
#             modifiers: mergedmodifiers,
#     }

colorUpdate : PatternType -> Result PatternType Str
colorUpdate = \ pattern ->
    when pattern is
        Allow (Plain pat ) -> 
            Ok (Color (Plain pat))
        Allow (Regex pat )->
            Ok (Color (Regex pat))
        _ -> Err "unsupported option"

blacklistUpdate : PatternType -> Result PatternType Str
blacklistUpdate = \  pattern ->
    when pattern is
        Allow pat->
            Ok (Blacklist pat)
        _ -> Err "unsupported option"
    
regexUpdate : PatternType -> Result PatternType Str
regexUpdate = \ pattern ->
    when pattern is
        Allow (Plain pat) ->
            Ok (Allow (Regex pat))
        Color (Plain pat) ->
            Ok (Color (Regex pat))
        Blacklist (Plain pat) ->
            Ok (Blacklist (Regex pat))
        _ -> Err "unsupported option"

createSection : ParsingResultType, ConfigType -> Result ConfigType Str
createSection = \  parsResult, config ->
    when (config.command, config.patterns ) is
        (Search, [pattern ]) | (None, [pattern ]) ->
            when pattern is 
                Color _ -> Err "don't mix color and command"
                _ ->      
                    when Regex.getValue [0] 0 parsResult.captured is 
                        Ok operation ->
                            arg1Result = Regex.getValue [1] 0 parsResult.captured  
                            when arg1Result  is 
                                Ok  arg1 -> 
                                    when operation is 
                                        [ 94 ] -> 
                                            Ok {
                                                config &
                                                command:
                                                    SearchSection
                                                        [{
                                                        before: (Utils.asciiArrayToNumber arg1 Str.toNat),
                                                        after: (Utils.asciiArrayToNumber arg1 Str.toNat),  
                                                        pattern: pattern}],
                                                modifiers : Set.empty {},
                                                patterns : [], 
                                                }
                                        [ 60 ] ->
                                            Ok {
                                                config &
                                                command:
                                                    SearchSection
                                                        [{
                                                        before: (Utils.asciiArrayToNumber arg1 Str.toNat),
                                                        after: 0,
                                                        pattern : pattern}],
                                                modifiers : Set.empty {},
                                                patterns : [], 
                                                }
                                        [ 62 ] -> 
                                            Ok {
                                                config &
                                                command:
                                                    SearchSection
                                                        [{
                                                        before : 0,
                                                        after: (Utils.asciiArrayToNumber arg1 Str.toNat),
                                                        pattern : pattern}],
                                                modifiers : Set.empty {},
                                                patterns : [], 
                                                }
                                        _ -> Err "wrong syntax"                           
                                Err message ->  Err message 
                        _ -> Err "wrong syntax" 
        _ -> Err "you try to put to many command " 

createPatternToPattern : ParsingResultType, ConfigType -> Result ConfigType Str
createPatternToPattern = \  parsResult, config ->
    { command: com, modifiers : mod, patterns : lst } = config
    when com is
        Search  ->  
            when ( 
                Regex.getValue [0] 0 parsResult.captured,
                Regex.getValue [1] 0 parsResult.captured,
                Regex.getValue [2] 0 parsResult.captured,
                Regex.getValue [3] 0 parsResult.captured)  is
                (Ok pat1,Ok pat2, Ok pat3, Ok pat4) ->
                    when (pat1, pat2, pat3, pat4) is 
                        ([_,..],[_,..],[_,..],[_,..])->
                            Ok { config & command : FromPatternToPattern (Allow (Regex (Utils.utfToStr pat2))) (Allow (Regex (Utils.utfToStr pat4) )) }
                        ([],[_,..],[_,..],[_,..])->
                            Ok { config & command : FromPatternToPattern (Allow (Plain(Utils.utfToStr pat2))) (Allow (Regex (Utils.utfToStr pat4))) }
                        ([_,..],[_,..],[],[_,..])->
                            Ok { config & command : FromPatternToPattern (Allow (Regex (Utils.utfToStr pat2))) (Allow (Plain (Utils.utfToStr pat4))) }
                        ([],[_,..],[],[_,..])->
                            Ok { config & command : FromPatternToPattern (Allow (Plain (Utils.utfToStr pat2))) (Allow (Plain (Utils.utfToStr pat4))) }
                        _ ->
                            Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"
                _ -> Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"
        _ -> Ok config

mergeConfigs : ConfigType, ConfigType  -> ConfigType
mergeConfigs = \configDest, newConfig ->
    updateCommand  = 
        when (configDest.command, newConfig.command ) is 
            (_,None) -> configDest.command
            (None, _ ) | (Search,_)-> newConfig.command
            (SearchSection sectionDest, SearchSection sectionNew) -> 
                SearchSection (List.concat sectionDest sectionNew)
            _ -> configDest.command
            
    mergedlimit = List.concat  configDest.limit newConfig.limit   

    mergedmodifiers = Set.union configDest.modifiers newConfig.modifiers
    mergedPatterns = List.concat  configDest.patterns newConfig.patterns

    {
        configDest  & 
            command : updateCommand,
            limit : mergedlimit, 
            patterns: mergedPatterns, 
            modifiers: mergedmodifiers,
    }

recoverConfigFromInput : List Str -> Result ConfigType Str
recoverConfigFromInput = \commandLst ->
    emptyConfig = (State.createConfigInstance [] Search (Set.empty {}) [] )
    if List.isEmpty commandLst then 
        Ok emptyConfig
    else  
        joined = 
            Str.joinWith  commandLst " "
            |> Str.concat  " "
            
        when Commands.runParser joined  { current : fullCommandMiniParser, data: { queue : [FullCommands], content : [] }, regexMagic : regexMagic }  is 
            Ok parserData ->
                Commands.updateConfig emptyConfig parserData.content
            Err mess -> Err mess 

replaceTilde : StateType, Str  -> Str
replaceTilde = \state, str -> 
    systemData = State.getSystemData state
    Str.replaceEach str "~" systemData.homePath 

handleUserCommand : StateType, Str -> Task StateType * 
handleUserCommand = \ state, commandPatRaw ->
    commandPat  = replaceTilde state commandPatRaw
    commandLst = Utils.tokenize commandPat
    mode = State.getAppMode state
    pickAndExecute : List Str -> Task StateType * 
    pickAndExecute = \ comLst ->
    
        buildExeConfig : List Str -> Task StateType * 
        buildExeConfig = \ commands -> 
            configResult = recoverConfigFromInput commands
            when configResult is
                Ok config -> 
                    toDisplay =
                        State.getFile state
                        |> SearchText.evalSearch config

                    _<- File.writeUtf8 (Path.fromStr "terminal_out.txt") toDisplay.terminal |>Task.attempt
                    _<- File.writeUtf8 (Path.fromStr "raw_out.txt") toDisplay.raw |>Task.attempt
                    Task.ok (State.setCommandOutput state toDisplay.terminal) 
                Err message -> 
                    Task.ok (State.setCommandOutput state message) 
        
        if mode == System then 
            exeState <- System.executeCommand state comLst |> Task.await
            Task.ok exeState
        else if mode == Search then 
            magic = Regex.regexMagic
            List.walkUntil comLst (Ok []) ( \ lstRes, command ->
                when  lstRes is
                    Ok lst ->
                        when Regex.parseStrMagic command "^fc@(.+)\$" magic is 
                            Ok parsed ->
                                if parsed.matchFound == Bool.true then
                                    #when Regex.getValue [0] 0 parsed.captured,is   #  accidental ',' crashes compiler
                                    when Regex.getValue [0] 0 parsed.captured is 
                                        Ok fileName ->
                                            Continue (Ok (List.append lst (Utils.utfToStr fileName)))
                                        _ ->
                                            Break (Err "weird problem in \(command) evaluation")
                                else 
                                    Continue lstRes
                            Err message -> Break (Err message)
                    Err message -> Break (Err message)
            )
            |> (\ fileResult -> 
                when fileResult is 
                    Ok fileLst ->
                        if List.isEmpty fileLst then
                            buildExeConfig comLst
                        else 
                            gatherPat = 
                                List.walk fileLst  (Task.ok []) (\ result, file ->
                                    lstResult <- result |> Task.attempt
                                    when lstResult is
                                        Ok  lst ->  
                                            #when  I try to use below it crashes
                                            #comFromFileResult <- System.loadCommands "file" |> Task.attempt 
                                            commandsResult <- File.readUtf8 (Path.fromStr file) |> Task.attempt
                                            when commandsResult is 
                                                Ok commands -> 
                                                    Task.ok  (List.concat  lst (Utils.tokenize commands))
                                                Err _ -> Task.err  "fail to load commands"
                                        Err message ->  result
                                )
                            comFromFilesResult <-gatherPat |> Task.attempt
                            when comFromFilesResult is 
                                Ok comFromFiles ->  
                                    buildExeConfig (List.concat comLst comFromFiles)
                                Err message -> Task.ok  (State.setCommandOutput state message)
                            
                    Err message ->Task.ok (State.setCommandOutput state message)
                    )

        else # Search 
            Task.ok state

    when commandLst is 
        [command] ->
            if command == quitCommand then
                Task.ok (State.setAppMode state Quitting)
            else if command == "help@" then
                Task.ok (State.setCommandOutput state helpStr)
            else
                if mode == System then 
                    if Str.startsWith command "fm@" == Bool.true then
                        when Str.splitFirst command "fm@" is 
                            Ok splitted -> 
                                fileResult <- File.readUtf8 (Path.fromStr splitted.after)  |> Task.attempt
                                when fileResult is
                                    Ok file ->
                                        Task.ok (
                                            
                                            State.setCommandOutput state  "Enter file analyze mode\n\rEnter filters and execute search"  
                                            |> State.setFile  (Utils.filterEmpty ( Utils.tokenizeNewLine file ) )
                                            |> State.setAppMode Search
                                            |> System.switchHistory Search
                                            |> State.updatePrompt )
                                    Err _ ->
                                        Task.ok (State.setCommandOutput state  "can't load file \(splitted.after)")
                            Err _ -> Task.ok state
                    else
                        pickAndExecute commandLst 
                else if mode == Search then
                    if command == "sm@" then
                        Task.ok (
                            State.setCommandOutput state  "Enter system command mode"  
                            |> State.setAppMode System
                            |> System.switchHistory System
                            |> State.updatePrompt )
                    else
                        pickAndExecute commandLst
                else # Quitting 
                    Task.ok state
        _ ->  pickAndExecute commandLst

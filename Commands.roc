interface Commands
    exposes [handleUserCommand,quitCommand, replaceTilde, recoverConfigFromInput]
    imports [
        pf.File,
        pf.Path,
        pf.Task.{ Task },
        Utils,
        Regex,
        SearchText,
        Regex.{ParsingResultType, regexMagic},
        State,
        System,
        State.{StateType, PatternType, ModifiersType, CommandType, ConfigType},
        "help" as helpStr : Str,
    ]

quitCommand : Str
quitCommand =  "peekQuit" 

dummyFun = \  parsResult, config ->
    Ok config


colorTag : ParsingResultType, ConfigType -> Result ConfigType Str
colorTag = \  parsResult, config ->
    when (config.command, config.patterns) is
        (None, [Allow pat]) ->
            Ok { config & command : Search, patterns : [Color pat ] }
        (Search, [Regex (Allow pat)])-> 
            Ok { config & patterns : [Regex (Color pat) ] }
        _ -> Ok config

regex : ParsingResultType, ConfigType -> Result ConfigType Str
regex = \  parsResult, config ->
    when (config.command, config.patterns) is
        (None, [Allow pat]) ->
            Ok { config & command : Search, patterns : [Regex (Allow pat) ] }
        (Search, [Color pat]) -> 
            Ok { config & patterns : [Regex (Color pat) ] }
        (Search, [Blacklist pat]) -> 
            Ok { config & patterns : [Regex (Blacklist pat) ] }
        (SearchSection {before: head, after : tail, pattern : (Allow pat)}, []) ->
            Ok { config & command: SearchSection {before:  head , after:  tail, pattern : (Regex (Allow pat) ) }}
        _ -> Ok config

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
                                                        {
                                                        before: (Utils.asciiArrayToNumber arg1 Str.toNat),
                                                        after: (Utils.asciiArrayToNumber arg1 Str.toNat),  
                                                        pattern: pattern},
                                                modifiers : Set.empty {},
                                                patterns : [], 
                                                }
                                        [ 60 ] ->
                                            Ok {
                                                config &
                                                command:
                                                    SearchSection
                                                        {
                                                        before: (Utils.asciiArrayToNumber arg1 Str.toNat),
                                                        after: 0,
                                                        pattern : pattern},
                                                modifiers : Set.empty {},
                                                patterns : [], 
                                                }
                                        [ 62 ] -> 
                                            Ok {
                                                config &
                                                command:
                                                    SearchSection
                                                        {
                                                        before : 0,
                                                        after: (Utils.asciiArrayToNumber arg1 Str.toNat),
                                                        pattern : pattern},
                                                modifiers : Set.empty {},
                                                patterns : [], 
                                                }
                                        _ -> Err "wrong syntax"                           
                                Err message ->  Err message 
                        _ -> Err "wrong syntax" 
        _ -> Err "you try to put to many command " 


createNumberLines : ParsingResultType, ConfigType -> Result ConfigType Str
createNumberLines = \  parsResult, config ->
    Ok { config & modifiers : Set.insert config.modifiers NumberLines }
    
andMode : ParsingResultType, ConfigType -> Result ConfigType Str
andMode = \  parsResult, config ->
    when Regex.getValue [0] 0 parsResult.captured is
        Ok patterns ->
            inConfigResult = 
                Utils.tokenize (Utils.utfToStr patterns)
                |> recoverConfigFromInput 
            when inConfigResult is 
                Ok inConfig ->
                    if inConfig.command == Search then                    
                        (
                        List.walkTry inConfig.patterns [] (\ andLst, pattern ->   
                            when  pattern is 
                                Allow pat ->  
                                   Ok (List.append andLst (Allow pat))
                                Blacklist pat -> 
                                    Ok  (List.append andLst (Blacklist pat))
                                Regex (Allow pat) ->
                                    Ok  (List.append andLst (Regex (Allow pat)))
                                Regex (Blacklist pat) -> 
                                    Ok (List.append andLst (Regex (Blacklist pat)))
                                _ -> Err "color cannot be used in and construction"
                        ))
                        |> (\ andPatternResult ->
                            when andPatternResult  is 
                                Ok lst ->
                                    if config.command == None then
                                        Ok {config & patterns : List.append config.patterns (LogicalAnd lst), command : Search}
                                    else
                                        Ok {config & patterns : List.append config.patterns (LogicalAnd lst)}
                                Err message -> Err message   
                        )
                    else 
                        Err "Error in parsing and@ command"
                Err _ ->  Err "Error in parsing and@ command"
        _ -> Err "Error in parsing and@ command"

createBlackListed : ParsingResultType, ConfigType -> Result ConfigType Str
createBlackListed = \  parsResult, config ->
    when (config.command, config.patterns) is
        (None, [ Allow pat ])  ->
                Ok { config & command : Search, patterns : [Blacklist pat] }
        (Search, [Regex ( Allow pat ) ])  ->
                Ok { config & patterns : [Regex ( Blacklist pat)] }
        _ -> Ok  config


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
                            Ok { config & command : FromPatternToPattern (Regex (Allow (Utils.utfToStr pat2))) (Regex (Allow (Utils.utfToStr pat4) )) }
                        ([],[_,..],[_,..],[_,..])->
                            Ok { config & command : FromPatternToPattern (Allow (Utils.utfToStr pat2)) (Regex (Allow (Utils.utfToStr pat4))) }
                        ([_,..],[_,..],[],[_,..])->
                            Ok { config & command : FromPatternToPattern (Regex (Allow (Utils.utfToStr pat2))) (Allow (Utils.utfToStr pat4)) }
                        ([],[_,..],[],[_,..])->
                            Ok { config & command : FromPatternToPattern (Allow (Utils.utfToStr pat2)) (Allow (Utils.utfToStr pat4)) }
                        _ ->
                            Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"
                _ -> Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"
        _ -> Ok config
        
        
createLineToLine : ParsingResultType, ConfigType -> Result ConfigType Str
createLineToLine = \  parsResult, config ->
    #{ command: com, modifiers : mod, patterns : lst } = config
      
    when ( 
        Regex.getValue [0] 0 parsResult.captured,
        Regex.getValue [1] 0 parsResult.captured)  is
            (Ok pat1,Ok pat2) ->
                when (pat1, pat2) is 
                    (['s'],['e'])->
                        Ok { config & limit : List.append config.limit (FromLineToLine  1 -1) }
                    (['s'],val)->
                        Ok { config & limit : List.append config.limit (FromLineToLine  1 (Utils.asciiArrayToNumber val Str.toI32)) }
                    (val,['e'])->
                        Ok { config & limit : List.append config.limit (FromLineToLine  (Utils.asciiArrayToNumber val Str.toI32) -1) }
                    (valStart,valEnd)->
                        Ok { config & limit : List.append config.limit (FromLineToLine  (Utils.asciiArrayToNumber valStart Str.toI32)  (Utils.asciiArrayToNumber valEnd Str.toI32)) }
                    _ ->
                        Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"
            _ -> Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"
    
    
mergeConfigs : ConfigType, ConfigType  -> ConfigType
mergeConfigs = \configDest, newConfig ->
    updateCommand  = 
        if configDest.command == None ||
           (configDest.command == Search && newConfig.command != None ) then
            newConfig.command
        else 
            configDest.command 
            
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

    
handleOthers : ParsingResultType, ConfigType -> Result ConfigType Str
handleOthers = \  parsResult, config ->
    when (Regex.getValue [0, 0] 0 parsResult.captured,
        Regex.getValue [0, 1] 0 parsResult.captured ) is
        (Ok modifiers, Ok pattern) ->
            # bit arbitrary and messy but consider that wrong command is just allow pattern, (later meybe change  this to show warning)
            if List.isEmpty modifiers == Bool.true then
                Ok { config & patterns : List.append config.patterns (Allow (Utils.utfToStr pattern)) }
            else if List.isEmpty pattern == Bool.true then
                Ok { config & patterns : List.append config.patterns (Allow (Utils.utfToStr modifiers)) }
            else
                modifierAnalysis (Utils.utfToStr modifiers) (State.createConfig [] None (Set.empty {}) [Allow (Utils.utfToStr pattern)] )
                |> ( \ partialConfigResult ->
                    merged = 
                        modifiers
                        |> List.concat pattern
                        |> Utils.utfToStr
                    when partialConfigResult is 
                        Ok partialConfig ->
                            # merge current subcommand to config
                            if partialConfig.command == None then
                                Ok {config & patterns : List.append config.patterns (Allow merged)}
                            else
                                Ok (mergeConfigs config  partialConfig)
                        Err message -> 
                            Err ("Error when processing \(Utils.utfToStr parsResult.matched) comand")
                )
        _ -> Err "Error when processing \(Utils.utfToStr parsResult.matched) comand"

modifiersHandlers : Dict Str ( ParsingResultType, ConfigType -> Result ConfigType Str)
modifiersHandlers =
    Dict.empty {}
    |> Dict.insert "^[cC]" colorTag
    |> Dict.insert "^[rR]" regex
    |> Dict.insert "^(\\^)(\\d+)" createSection
    |> Dict.insert "^(>)(\\d+)" createSection
    |> Dict.insert "^(<)(\\d+)" createSection
    |> Dict.insert "^[bB]" createBlackListed


modifierAnalysis : Str, ConfigType -> Result ConfigType Str
modifierAnalysis = \ word, inState -> 
    if Str.isEmpty  word then 
        Ok inState
    else  
        Dict.keys modifiersHandlers
        |> List.walkUntil (Ok inState) (\ state, pattern ->
            when state  is 
                Ok config ->
                        when Regex.parseStrMagic word pattern inState.regexMagic is 
                            Ok parsed ->
                                if parsed.matchFound == Bool.true then
                                    when Dict.get modifiersHandlers pattern  is
                                        Ok handler ->
                                            modifiedConfResult = handler parsed config
                                            when modifiedConfResult is
                                                Ok modifiedConf -> Break (modifierAnalysis (Utils.utfToStr parsed.left) modifiedConf)
                                                Err _ -> Break (Err "internal application error, during comand \(word) analysis")
                                        Err _ -> 
                                            Break (Err "internal application error, during comand \(word) analysis")
                                else 
                                    Continue state
                            Err message -> Break (Err message)
                    
                Err message -> Break (Err message)
        )


commandsToHandlers : Dict Str ( ParsingResultType, ConfigType -> Result ConfigType Str)
commandsToHandlers =
    Dict.empty {}
    |> Dict.insert "^[Nn][Ll]@" createNumberLines
    #|> Dict.insert "dsdsa" (\ type, config -> andMode type, config )  # those lines create cycles I am not sure they should be  
    #|> Dict.insert "dsdsa"  andMode 
    |> Dict.insert "([Rr])?@(.+)->([Rr])?@(.+)" createPatternToPattern
    |> Dict.insert "^(\\d+|s)->(\\d+|e)@\$" createLineToLine
    |> Dict.insert "(([^@]+@)?(.*))" handleOthers
    
simplifiedSyntax : Dict Str Str
simplifiedSyntax = 
    Dict.empty {}

commandAnalysis : Str, ConfigType -> Result ConfigType Str
commandAnalysis = \ word, inConfig -> 
    Dict.keys commandsToHandlers
    |> List.walkUntil (Ok inConfig) (\ state, pattern ->
        when state  is 
            Ok config -> 
                when Regex.parseStrMagic word pattern inConfig.regexMagic is
                    Ok parsed -> 
                        if parsed.matchFound == Bool.true then
                            when Dict.get commandsToHandlers pattern  is
                                Ok handler ->
                                    Break (handler parsed config)
                                Err _ -> 
                                    Break (Err "command  \(word) evaluation error")
                        else 
                            Continue state
                    Err message -> Break (Err message)
            Err message -> Break (Err message)
         )

recoverConfigFromInput : List Str -> Result ConfigType Str
recoverConfigFromInput = \filterStr ->
    andEvaluatedResult = 
        Str.joinWith  filterStr " "
        |> evalAndCommand (State.createConfig [] Search (Set.empty {}) [] ) 
        
    evalAndCommand : Str, ConfigType -> Result ( ConfigType, Str, Str ) Str
    evalAndCommand =  \ inCommand, config -> 
        # While the design is not ideal, it was not something that I had anticipated.
        when Regex.parseStrMagic inCommand "[Aa][Nn][Dd]@\\(" config.regexMagic is
            Ok allParsed -> 
                if allParsed.matchFound == Bool.true then
                    processed = evalAndCommand (Utils.utfToStr allParsed.left) config  
                    when processed is 
                        Ok lowerProc ->
                            when Regex.parseStrMagic lowerProc.1 "(.+)@\\)" config.regexMagic is
                                Ok parsed ->
                                    if parsed.matchFound == Bool.true then
                                        when andMode parsed lowerProc.0 is
                                            Ok updatedConfig ->
                                                Ok  
                                                    (
                                                        updatedConfig,
                                                        (Utils.utfToStr allParsed.missed),
                                                        (Str.concat (Utils.utfToStr parsed.left) lowerProc.2)
                                                    )   
                                            Err message -> Err "Error in parsing and@ command" 
                                    else
                                        Err "Error in parsing and@ command"          
                                Err message -> Err "Error in parsing and@ command" 
                        _ -> Err "Error in parsing and@ command"                     
                else
                    Ok (config, inCommand, "")
            Err message -> Err message 
            
            
    when andEvaluatedResult is 
        Ok andEvaluated -> 
            
            List.walkTry (Utils.tokenize (Str.concat  andEvaluated.1 andEvaluated.2)) andEvaluated.0  (\ config, word ->
                when commandAnalysis word config is 
                    Ok updatedConfig ->  Ok updatedConfig
                    Err message ->  Err message
                )
        Err message -> Err message

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

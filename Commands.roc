interface Commands
    exposes [
        handleUserCommand,
        quitCommand, 
        replaceTilde, 
        recoverConfigFromInput, 
        runParser,
        configMiniParser,
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

MiniParserDataType : [ Simple SimpleType, ParserConfig ParserConfigType, Condition Bool ]

ParserOutcomeType : [ Completed MiniParserDataType, Continue MiniParserDataType, Error ]  

MiniParserType : { handlers: Dict Str (ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str), data : MiniParserDataType, left : Str }

ParserPhasesType : [ OpenAnd, EndAnd, AndPattern, AndConfig, Config, Pattern ]

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

    when data is 
        Simple simpleData ->
            when parser.data.queue is 
                [AndPattern, ..] ->
                    updatedQueue = 
                        parser.data.queue
                        |> List.dropFirst 1
                        |> List.prepend EndAnd
                        
                    when parser.data.content is
                        [] ->  
                            Err  "unknown problem with command processing" 
                        [.. as head, last] ->
                            elems = List.append head { last & pattern : simpleData }
                            Ok
                                { parser &
                                    current : { handlers: closeANDHandlers, data : Condition Bool.false, left : "" },
                                    data : { queue : updatedQueue, content : elems } }
                _ -> 
                    Err "not implemented" 
        ParserConfig configDataProcessed ->
            updatedQueue = 
                parser.data.queue
                |> List.dropFirst 1
                |> List.prepend AndPattern
            
            if Set.isEmpty configDataProcessed.options then
                when parser.data.queue is 
                    [AndConfig, ..] -> 
                        Ok  
                            { parser &
                                current : { handlers: simpleHandlers, data : Simple  "", left : "" },
                                data : { queue : updatedQueue, content : parser.data.content} } 
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
                            current : { handlers: simpleHandlers, data : Simple  "", left : "" },
                            data : { queue : updatedQueue, content : (List.append head elem)} }
        
                (if Set.contains configDataProcessed.options LogicalAnd == Bool.true then

                    entryHandlers =
                        Dict.empty {}
                        |> Dict.insert "^\\("  conditionHit
                    { parser &
                            current : { handlers: entryHandlers, data : Condition Bool.false, left : "" },
                            data : { queue : [OpenAnd], content : parser.data.content } }
                else
                    { parser & data : { queue : [], content : parser.data.content } }
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
                                      
                    updatedQueue = 
                        parser.data.queue
                        |> List.dropFirst 1
                        |> List.prepend EndAnd                    
                    Ok 
                        { parser &
                            current : { handlers: closeANDHandlers, data : Condition Bool.false, left : "" },
                            data : { queue : updatedQueue, content : parser.data.content } }
                
                [EndAnd, .. as tail] -> 
                    if cond == Bool.true then 
                        if List.isEmpty parser.data.content then 
                            Err "error in and command, empty pattern section"
                        else
                            when tail is 
                                [] ->
                                    dbg "inject"
                                    Ok {parser & data : { queue : [], content : parser.data.content }  }   
                                _ -> 
                                    dbg "deJect"
                                    evaluate data  {parser & data : { queue : [], content : parser.data.content }  }
                    else 
                        andHandlers =
                            Dict.empty {}
                            |> Dict.insert "^[rR]" regexHandler
                            |> Dict.insert "^[bB]" blackListedHandler
                            |> Dict.insert "^@" concludeHandler
                            
                        andConfigData = { exclusive: Dict.empty {}, options : Set.empty {} }
                        updatedQueue = 
                            parser.data.queue
                            |> List.dropFirst 1
                            |> List.prepend AndConfig
                        content = List.append parser.data.content { config: Set.empty {}, pattern : "" }
                        Ok { parser &
                            current : { handlers: andHandlers, data : ParserConfig andConfigData, left : "" },
                            data : { queue : updatedQueue, content : content } }
                _ -> 
                    Err "unknown problem with command processing"

isCompleted : MiniParserDataType -> Bool
isCompleted = \ parserData -> 
    Bool.true

isParserCompleted : ParserDataType -> Bool
isParserCompleted = \ parserData -> 
    List.isEmpty parserData.queue

runParser : Str, ParserType -> Result ParserDataType Str
runParser = \ input, parser -> 
    dbg  parser.data
    loopCommand : Str, MiniParserType -> Result MiniParserType Str
    loopCommand = \ feedRaw, miniParser ->
        feed = Str.trimStart feedRaw
        if Str.isEmpty feed then
            if isCompleted miniParser.data then
                Ok miniParser                
            else
                Err "error while processing \(input)"
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
                        if isParserCompleted alteredParser.data then
                            Ok alteredParser.data
                        else 
                            runParser parsed.left alteredParser
                    Err message -> Err message  

            Err message -> Err message  )                
    
# createConfig :     ParserOutType
# createConfig \ = 


# commandsToHandlers : Dict Str ( ParsingResultType, ConfigType -> Result ConfigType Str)
# commandsToHandlers =
#     Dict.empty {}
#     |> Dict.insert "^[Nn][Ll]@\\s" createNumberLines
#     #|> Dict.insert "dsdsa" (\ type, config -> andMode type, config )  # those lines create cycles I am not sure they should be  
#     #|> Dict.insert "dsdsa"  andMode 
#     |> Dict.insert "^([Rr])?@(.+)->([Rr])?@(\\S+)\\s" createPatternToPattern
#     |> Dict.insert "^(\\d+|s)->(\\d+|e)@\\s" createLineToLine
#     |> Dict.insert "^(([^@]+@)?(\\S*)\\s)" handleOthers


colorTag : ParsingResultType, ConfigType -> Result ConfigType Str
colorTag = \  parsResult, config ->
    when (config.command, config.patterns) is
        (None, [Allow (Plain pat)])->
            Ok { config & command : Search, patterns : [Color (Plain pat) ] }
        (Search, [Allow (Regex  pat)])-> 
            Ok { config & patterns : [Color (Regex pat) ] }
        _ -> Ok config

regex : ParsingResultType, ConfigType -> Result ConfigType Str
regex = \  parsResult, config ->
    when (config.command, config.patterns) is
        (None, [Allow (Plain pat)]) ->
            Ok { config & command : Search, patterns : [Allow (Regex pat) ] }
        (Search, [Color (Plain pat)]) -> 
            Ok { config & patterns : [Color (Regex pat) ] }
        (Search, [Blacklist (Plain pat)]) -> 
            Ok { config & patterns : [Blacklist (Regex pat) ] }
        (SearchSection patternLst, _) ->
            when patternLst is 
                [.., {before: head, after : tail, pattern : (Allow (Plain pat))}] ->
                    Ok { config & command: SearchSection (Utils.modifyLastInList  patternLst {before:  head , after:  tail, pattern : (Allow (Regex pat) ) } )}
                _ -> Err "section error"
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
        (None, [ Allow (Plain pat) ])  ->
                Ok { config & command : Search, patterns : [Blacklist (Plain pat)] }
        (Search, [Allow (Regex pat ) ])  ->
                Ok { config & patterns : [Blacklist (Regex pat)] }
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

    
handleOthers : ParsingResultType, ConfigType -> Result ConfigType Str
handleOthers = \  parsResult, config ->
    when (Regex.getValue [0, 0] 0 parsResult.captured,
        Regex.getValue [0, 1] 0 parsResult.captured ) is
        (Ok modifiers, Ok pattern) ->
            # bit arbitrary and messy but consider that wrong command is just allow pattern, (later meybe change  this to show warning)
            if List.isEmpty modifiers == Bool.true then
                Ok { config & patterns : List.append config.patterns (Allow (Plain (Utils.utfToStr pattern))) }
            else if List.isEmpty pattern == Bool.true then
                Ok { config & patterns : List.append config.patterns (Allow (Plain (Utils.utfToStr modifiers))) }
            else
                modifierAnalysis (Utils.utfToStr modifiers) (State.createConfig [] None (Set.empty {}) [Allow (Plain (Utils.utfToStr pattern))] )
                |> ( \ partialConfigResult ->
                    merged = 
                        modifiers
                        |> List.concat pattern
                        |> Utils.utfToStr
                    when partialConfigResult is 
                        Ok partialConfig ->
                            # merge current subcommand to config
                            if partialConfig.command == None then
                                Ok {config & patterns : List.append config.patterns (Allow (Plain merged))}
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
        
preventDoubleAND : Str, MagicType -> Bool
preventDoubleAND = \ str, magic ->
    when Regex.parseStrMagic str "[a][n][d]@" magic is 
        Ok parsed ->
            if parsed.matchFound == Bool.true then
                when Regex.parseStrMagic (Utils.utfToStr parsed.left) "[a][n][d]@" magic is
                    Ok secParse ->
                        Bool.not secParse.matchFound
                    Err _ -> 
                        Bool.false
            else 
                Bool.true
        Err _ -> Bool.false

findCommandCandidate : List ParsingResultType, (Str, MagicType -> Bool), MagicType -> ParsingResultType
findCommandCandidate = \  candidates, isSane, magic ->
    if List.isEmpty candidates == Bool.true then 
        Regex.createParsingRecord [] Inactive No
    else 
        # pick longest sane candidate
        List.walk candidates (Regex.createParsingRecord [] Inactive No) (\ candidate, current  ->
            if List.len candidate.matched < List.len current.matched && 
               isSane (Utils.utfToStr current.matched) magic == Bool.true then
                current
            else
                candidate
        )

commandsToHandlers : Dict Str ( ParsingResultType, ConfigType -> Result ConfigType Str)
commandsToHandlers =
    Dict.empty {}
    |> Dict.insert "^[Nn][Ll]@\\s" createNumberLines
    #|> Dict.insert "dsdsa" (\ type, config -> andMode type, config )  # those lines create cycles I am not sure they should be  
    #|> Dict.insert "dsdsa"  andMode 
    |> Dict.insert "^([Rr])?@(.+)->([Rr])?@(\\S+)\\s" createPatternToPattern
    |> Dict.insert "^(\\d+|s)->(\\d+|e)@\\s" createLineToLine
    |> Dict.insert "^(([^@]+@)?(\\S*)\\s)" handleOthers
    
simplifiedSyntax : Dict Str Str
simplifiedSyntax = 
    Dict.empty {}

commandAnalysis : Str, ConfigType -> Result {config: ConfigType, left : Str} Str
commandAnalysis = \ line, inConfig ->
    cleanLine = Str.trimStart line
    if Str.isEmpty cleanLine then
        Ok {config: inConfig, left : cleanLine}
    else 
        Dict.keys commandsToHandlers
        |> List.walkUntil (Ok {config: inConfig, left: "" }) (\ state, pattern ->
            when state  is 
                Ok {config: config,left: _} -> 
                    when Regex.parseStrMagic (Str.trimStart line) pattern inConfig.regexMagic is
                        Ok parsed -> 
                            if parsed.matchFound == Bool.true then
                                when Dict.get commandsToHandlers pattern  is
                                    Ok handler ->
                                        when (handler parsed config) is 
                                            Ok outConfig -> 
                                                Break (Err "command  \(line) evaluation error")
                                                #Break (Ok {config: outConfig, left: Utils.utfToStr parsed.left })
                                            Err _ -> Break (Err "command  \(line) evaluation error")
                                    Err _ -> 
                                        Break (Err "command  \(line) evaluation error")
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
            loopCommand :  Str, ConfigType -> Result ConfigType Str
            loopCommand = \ input, config ->
                if Str.isEmpty input then 
                    Ok config
                else
                    when commandAnalysis input config is 
                        Ok updatedConfig ->  loopCommand updatedConfig.left updatedConfig.config
                        Err message ->  Err message            
            loopCommand (Str.concat  andEvaluated.1 andEvaluated.2) andEvaluated.0
            
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

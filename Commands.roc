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
        #Allow,
        Blacklist,
        Color,
        LogicalAnd,
        Region,
        FromLineToLine  I32 I32,
        Section Nat Nat,
        NumberLines ]

ParserConfigType : { exclusive: Dict OperationType (List OperationType), options : Set OperationType }

MiniParserDataType : [ Simple SimpleType, ParserConfig ParserConfigType, Condition Bool, Other [FromLineToLine  I32 I32, NumberLines, None] ]

ParserOutcomeType : [ Completed MiniParserDataType, Continue MiniParserDataType, Error ]

MiniParserType : { handlers: Dict Str (ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str), data : MiniParserDataType, left : Str }

ParserPhasesType : [ OpenAnd, EndAnd, AndPattern, AndConfig, Config, Pattern, FullCommands ]

ParserOutType : { config:  Set OperationType, pattern : Str }

ParserDataType : { queue : List ParserPhasesType, content: List ParserOutType }

ParserType : { current : MiniParserType, data: ParserDataType, regexMagic: MagicType }

insertTag : MiniParserDataType, OperationType -> ParserOutcomeType
insertTag = \ miniParserData, operation ->
    when miniParserData is
        ParserConfig configType ->
            when Dict.get configType.exclusive operation is
                Ok exclusiveLst ->
                    List.walkUntil exclusiveLst Bool.false (\ _status, tag ->
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
colorHandler = \ _parsed, miniParser ->
    Ok (insertTag miniParser Color)

regexHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
regexHandler = \ _parsed, miniParser ->
    Ok (insertTag miniParser Regex)

blackListedHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
blackListedHandler = \ _parsed, miniParser ->
    Ok (insertTag miniParser Blacklist)

logicalANDHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
logicalANDHandler = \ _parsed, miniParser ->
    Ok (insertTag miniParser LogicalAnd)

regionHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
regionHandler = \ _parsed, miniParser ->
    Ok (insertTag miniParser Region)

concludeHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
concludeHandler = \ _parsed, miniParserData ->
    Ok (Completed miniParserData)

sectionHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
sectionHandler = \ parsed, miniParser ->
    when Regex.getValue [0] 0 parsed.captured is
        Ok operation ->
            arg1Result = Regex.getValue [1] 0 parsed.captured
            when arg1Result  is
                Ok  arg1 ->
                    when operation is
                        [ 94 ] ->
                            Ok (insertTag miniParser
                                (Section
                                    (Utils.asciiArrayToNumber arg1 Str.toNat)
                                    (Utils.asciiArrayToNumber arg1 Str.toNat)))
                        [ 60 ] ->
                            Ok (insertTag miniParser
                                (Section
                                    (Utils.asciiArrayToNumber arg1 Str.toNat)
                                    0))
                        [ 62 ] ->
                            Ok (insertTag miniParser
                                (Section
                                    0
                                    (Utils.asciiArrayToNumber arg1 Str.toNat)))
                        _ -> Err "wrong syntax"
                _ -> Err "wrong syntax"
        _ -> Err "wrong syntax"

configHandlers : Dict Str (ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str)
configHandlers =
    Dict.empty {}
    |> Dict.insert "^[cC]" colorHandler
    |> Dict.insert "^[rR]" regexHandler
    |> Dict.insert "^(\\^)(\\d+)" sectionHandler
    |> Dict.insert "^(>)(\\d+)" sectionHandler
    |> Dict.insert "^(<)(\\d+)" sectionHandler
    |> Dict.insert "^[bB]" blackListedHandler
    |> Dict.insert "^[a][n][d]" logicalANDHandler
    |> Dict.insert "^->" regionHandler
    |> Dict.insert "^@" concludeHandler

configANDHandlers : Dict Str (ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str)
configANDHandlers =
    Dict.empty {}
    |> Dict.insert "^[rR]" regexHandler
    |> Dict.insert "^[bB]" blackListedHandler
    |> Dict.insert "^@" concludeHandler

fullCommandsHandlers : Dict Str (ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str)
fullCommandsHandlers =
    Dict.empty {}
    |> Dict.insert "^[Nn][Ll]@\\s" createNumberLines
    |> Dict.insert "^(\\d+|s)->(\\d+|e)@\\s" createLineToLine

    #|> Dict.insert "^([Rr])?@(.+)->([Rr])?@(\\S+)\\s" createPatternToPattern

fullCommandMiniParser =  { handlers: fullCommandsHandlers, data :  Other None, left : "" }

createNumberLines : createNumberLines, MiniParserDataType -> Result ParserOutcomeType Str
createNumberLines = \ _parsed, _miniParser ->
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
            _ -> Err "Command processing error : \(Utils.utfToStr parsResult.matched) comand"

simpleHandlers =
    Dict.empty {}
    |> Dict.insert "^(\\S+)->" simpleHandler # separate this case
    |> Dict.insert "^(\\S+)\\s" simpleHandler

simpleHandler : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
simpleHandler = \ parsed, _miniParserData ->
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

configANDParser =  { handlers: configANDHandlers, data :  ParserConfig configData, left : "" }

conditionHit : ParsingResultType, MiniParserDataType -> Result ParserOutcomeType Str
conditionHit = \ parsed, _data ->
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

                (FromLineToLine  val1 val2) ->
                    content = List.append  parser.data.content { config: Set.empty {} |> Set.insert (FromLineToLine  val1 val2), pattern : "" }
                    Ok
                        { parser &
                            current : fullCommandMiniParser,
                            data : { queue : updatedQueue |> List.prepend FullCommands, content : content } }
                NumberLines ->
                    content = List.append  parser.data.content { config: Set.empty {} |> Set.insert NumberLines, pattern : "" }
                    Ok
                        { parser &
                            current : fullCommandMiniParser,
                            data : { queue : updatedQueue |> List.prepend FullCommands, content : content } }
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
                                    current : fullCommandMiniParser,
                                    data : { queue : updatedQueue |> List.prepend FullCommands, content : elems } }
                _ ->
                    Err "not implemented"
        ParserConfig configDataProcessed ->
                updateParser : ParserType, OperationType -> ParserType
                updateParser = \ inParser, operation ->
                    when inParser.data.content  is
                    [] ->
                        inParser
                    [.. as head, last] ->
                        elem = { config: last.config |> Set.insert operation, pattern : last.pattern }
                        { inParser &
                            data : { queue : inParser.data.queue, content : (List.append head elem)} }
                content = List.append  parser.data.content { config: Set.empty {}, pattern : "" }
                (if Set.contains configDataProcessed.options LogicalAnd == Bool.true then
                    #content = List.append  parser.data.content { config: Set.empty {} |> Set.insert LogicalAnd, pattern : "" }
                    entryHandlers =
                        Dict.empty {}
                        |> Dict.insert "^\\("  conditionHit
                    { parser &
                            current : { handlers: entryHandlers, data : Condition Bool.false, left : "" },
                            data : { queue : [OpenAnd], content : content } }
                else
                    when parser.data.queue is
                        [AndConfig, ..] ->
                            { parser &
                                    current : { handlers: simpleHandlers, data : Simple  "", left : "" },
                                    data : { queue : (updatedQueue |> List.prepend AndPattern), content : content} }
                        [Config, ..] ->
                            { parser &
                                    current : { handlers: simpleHandlers, data : Simple  "", left : "" },
                                    data : { queue : (updatedQueue |> List.prepend Pattern), content : content} }
                        _ -> parser
                )
                |> ( \ inProcessParser  -> Set.walk configDataProcessed.options inProcessParser ( \ state, type ->
                        (updateParser state type)
                    ))
                |> ( \ inProcessParser -> Ok inProcessParser)
        Condition cond ->
            when parser.data.queue is
                [OpenAnd, ..] ->
                    Ok
                        { parser &
                            current : { handlers: closeANDHandlers, data : Condition Bool.false, left : "" },
                            data : { queue : updatedQueue |> List.prepend EndAnd, content : parser.data.content } }
                [AndConfig, ..] ->
                    content = List.append  parser.data.content { config: Set.empty {}, pattern : "" }
                    Ok
                        { parser &
                                    current : { handlers: simpleHandlers, data : Simple  "", left : "" },
                                    data : { queue : (updatedQueue |> List.prepend AndPattern), content : content } }
                [Config, ..] ->
                    content = List.append  parser.data.content { config: Set.empty {}, pattern : "" }
                    Ok
                        { parser &
                                    current : { handlers: simpleHandlers, data : Simple  "", left : "" },
                                    data : { queue : (updatedQueue |> List.prepend Pattern), content : content } }
                [FullCommands, ..] ->
                            Ok
                                { parser &
                                    current : configMiniParser,
                                    data : { queue : updatedQueue |> List.prepend Config, content : parser.data.content } }
                [EndAnd, .. as tail] ->
                    if cond == Bool.true then
                        if List.isEmpty parser.data.content then
                            Err "error in and command, empty pattern section"
                        else
                            content = List.append parser.data.content { config: Set.empty {}|> Set.insert LogicalAnd, pattern : "Stop" }
                            Ok
                                { parser &
                                    current : fullCommandMiniParser,
                                    data : { queue : updatedQueue |> List.prepend FullCommands, content : content } }
                            # when tail is
                            #     [] ->
                            #         Ok {parser & data : { queue : [], content : parser.data.content }  }
                            #     _ ->
                            #         evaluate data  {parser & data : { queue : [], content : parser.data.content }  }
                    else
                        Ok { parser &
                            current : configANDParser,
                            data : { queue : updatedQueue |> List.prepend AndConfig, content : parser.data.content } }
                _ ->
                    Err "unknown problem with command processing"

runParser : Str, ParserType -> Result ParserDataType Str
runParser = \ input, parser ->
    loopCommand : Str, MiniParserType -> Result MiniParserType Str
    loopCommand = \ feed, miniParser ->
        if Str.isEmpty feed then
            # if isCompleted miniParser.data then
            #    Ok miniParser
            # else
                Ok {miniParser & data : Condition Bool.false, left : input}
        else
            config = miniParser
            Dict.keys miniParser.handlers
            |> List.walkUntil (Ok {miniParser & data : Condition Bool.false} ) (\ state, pattern ->
                when state  is
                    Ok _ ->
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
                                                        Error ->
                                                            Continue ( Ok {miniParser & data : Condition Bool.false, left : input})
                                                Err _ -> Break (Err "error while processing \(input)")
                                        Err _ ->
                                            Break (Err "internal application error, during comand \(input) analysis")
                                else
                                    Continue (Ok {config & data : Condition Bool.false, left : input})
                            Err message -> Break (Err message)
                    Err message -> Break (Err message)
                    )
    loopCommand  (Str.trimStart input) parser.current
    |> ( \ parsedResult ->
        when parsedResult is
            Ok  parsed ->
                when evaluate parsed.data parser is
                    Ok alteredParser ->
                        if Str.isEmpty (Str.trimStart parsed.left) then
                            # validate if parsing correct
                            Ok alteredParser.data
                        else
                            runParser parsed.left alteredParser
                    Err message -> Err message

            Err message -> Err message  )

andTypesConversion : [Allow [ Plain Str, Regex Str], Blacklist [Plain Str, Regex Str]] -> PatternType
andTypesConversion = \ type ->
    when type is
        Allow (Plain str) -> Allow (Plain str)
        Allow (Regex str) -> Allow (Regex str)
        Blacklist (Plain str) -> Blacklist (Plain str)
        Blacklist (Regex str) -> Blacklist (Regex str)

andTypesConversionBack : PatternType -> Result [Allow [ Plain Str, Regex Str], Blacklist [Plain Str, Regex Str]] Str
andTypesConversionBack = \ type ->
    when type is
        Allow (Plain str) -> Ok (Allow (Plain str))
        Allow (Regex str) -> Ok (Allow (Regex str))
        Blacklist (Plain str) -> Ok (Blacklist (Plain str))
        Blacklist (Regex str) -> Ok (Blacklist (Regex str))
        _ -> Err "conversion impossible"

updateConfig : ConfigType, List ParserOutType -> Result ConfigType Str
updateConfig = \ config, parserDataLst ->
    if List.isEmpty parserDataLst == Bool.true then
        Ok config
    else

        modifyPattern: OperationType, PatternType -> Result [Pattern PatternType] Str
        modifyPattern = \ operation, pattern ->
            when operation is
                Color ->
                    when colorUpdate pattern is
                        Ok pat ->
                            Ok (Pattern pat)
                        Err _ -> Err "wrong usage of command"
                Regex ->
                        when regexUpdate pattern is
                            Ok pat ->
                                Ok (Pattern pat)
                            Err _ -> Err "wrong usage of command"
                Blacklist ->
                        when blacklistUpdate pattern is
                            Ok pat ->
                                Ok (Pattern pat)
                            Err _ -> Err "wrong usage of command"
                LogicalAnd ->
                        when andUpdate pattern is
                            Ok pat ->
                                Ok (Pattern pat)
                            Err _ -> Err "wrong usage of command"
                _ -> Err "unknown problem during command processing"

        updateRegionPat : PatternType, OperationType -> Result PatternType Str
        updateRegionPat = \ pattern, type ->
            when pattern is
                (Allow (LogicalAnd lst)) ->
                    when lst is
                        [] -> Err "unknown problem during command processing"
                        [.. as head, last ] ->
                            when modifyPattern type (andTypesConversion last) is
                                Ok (Pattern modifiedPat) ->
                                    when andTypesConversionBack modifiedPat is
                                        Ok convertPat ->
                                            Ok (Allow (LogicalAnd (List.append head convertPat)))
                                        Err _ -> Err "unknown problem during command processing"
                                Err message -> Err message
                _ ->
                    when type is
                        Regex | LogicalAnd ->
                            when modifyPattern type pattern is
                                Ok (Pattern modifiedPat) ->
                                    Ok modifiedPat
                                Err message -> Err message
                        Region -> Ok pattern
                        _ ->
                            Err "unknown problem during command processing"

        ( List.walkUntil parserDataLst (Ok (parserDataLst, None)) (\ state, data ->
            when state is
                Ok current ->
                    (if Set.contains data.config LogicalAnd &&
                       data.pattern == "Stop" then
                        when current.1 is
                            Region ( Init (Allow (LogicalAnd lst)), Empty ) ->
                                Continue (Ok ( List.dropFirst current.0 1, Region ( Init (Allow (LogicalAnd lst)), Init (Allow (Plain "") )) ))
                            _ ->
                                Break (Ok ( List.dropFirst current.0 1, current.1 ))
                    else
                        updated =
                            if Str.isEmpty data.pattern == Bool.true then
                                when current.1 is
                                    Region ( Init pattern, Empty ) ->
                                        Region ( Init pattern, Init (Allow (Plain "") ))
                                    _ ->
                                        current.1
                            else
                                when current.1 is
                                    None ->
                                        (Pattern (Allow (Plain data.pattern)))
                                    (Pattern (Allow (LogicalAnd lst))) ->
                                        (Pattern (Allow (LogicalAnd (List.append lst  (Allow (Plain data.pattern)) ))))
                                    (Pattern (Blacklist (LogicalAnd lst))) ->
                                        (Pattern (Blacklist (LogicalAnd (List.append lst  (Allow (Plain data.pattern)) ))))
                                    Section {before: before, after: after, pattern : Allow (LogicalAnd lst)} ->
                                        Section {before: before, after: after, pattern : Allow (LogicalAnd (List.append lst (Allow (Plain data.pattern))))}
                                    Region (pat1, pat2 )->
                                        when (pat1, pat2 ) is
                                            (Empty, Empty ) ->
                                                Region ( Init (Allow (Plain data.pattern)), Empty )
                                            (Init pattern, Empty ) ->
                                                when pattern is
                                                        Allow ( Regex "") ->
                                                            Region (Init (Allow (Regex data.pattern)), Empty)
                                                        (Allow (LogicalAnd lst)) ->
                                                            updatedPat =  Allow (LogicalAnd (List.append lst  (Allow (Plain data.pattern)) ))
                                                            Region (Init updatedPat, Empty)
                                                        _ ->
                                                            Region (Init pattern, Init (Allow (Plain data.pattern)) )
                                            (Init pattern1, Init pattern2 ) ->
                                                when pattern2 is
                                                        Allow ( Plain "") ->
                                                            Region (Init pattern1, Init (Allow (Plain data.pattern)))
                                                        Allow ( Regex "") ->
                                                            Region (Init pattern1, Init (Allow (Regex data.pattern)))
                                                        (Allow (LogicalAnd lst)) ->
                                                            updatedPat = Allow (LogicalAnd (List.append lst  (Allow (Plain data.pattern)) ))
                                                            Region (Init pattern1, Init updatedPat)
                                                        _ -> Region (pat1, pat2 )
                                            _ -> Region (pat1, pat2 )
                                    _ -> current.1
                        (List.walkTry (Set.toList data.config) updated (\ inState, type  ->
                            when inState is
                                None ->
                                    when type is
                                        Regex | Blacklist | Color | LogicalAnd ->
                                            modifyPattern type (Allow (Plain data.pattern))
                                        NumberLines ->
                                            Ok (Modifier NumberLines)
                                        FromLineToLine  from to ->
                                            Ok (Limit [FromLineToLine  from to])
                                        Region ->
                                            Ok (Region ( Empty, Empty ))
                                        Section before after ->
                                            Ok (Section {before: before, after: after, pattern : (Allow (Plain data.pattern))})
                                        _ -> Err "not supported command"
                                Pattern pat ->
                                    when pat is
                                        (Allow (LogicalAnd lst)) ->
                                            when lst is
                                                [] ->
                                                    when type is
                                                        Blacklist ->
                                                            modifyPattern type pat
                                                        Region ->
                                                            Ok (Region ( Init pat, Empty))
                                                        Section before after ->
                                                            Ok (Section {before: before, after: after, pattern : pat})
                                                        _ ->
                                                            Err "unknown problem during command processing"
                                                [.. as head, last ] ->
                                                    when modifyPattern type (andTypesConversion last) is
                                                        Ok (Pattern modifiedPat) ->
                                                            when andTypesConversionBack modifiedPat is
                                                                Ok convertPat ->
                                                                    Ok (Pattern (Allow (LogicalAnd (List.append head convertPat))))
                                                                Err _ -> Err "unknown problem during command processing"
                                                        Err message -> Err message

                                        (Blacklist (LogicalAnd lst)) ->
                                            when lst is
                                                [] -> Err "unknown problem during command processing"
                                                [.. as head, last ] ->
                                                    when modifyPattern type (andTypesConversion last) is
                                                        Ok (Pattern modifiedPat) ->
                                                            when andTypesConversionBack modifiedPat is
                                                                Ok convertPat ->
                                                                    Ok (Pattern (Blacklist (LogicalAnd (List.append head convertPat))))
                                                                Err _ -> Err "unknown problem during command processing"
                                                        Err message -> Err message
                                        _ ->
                                            when type is
                                                Regex | Blacklist | Color | LogicalAnd ->
                                                    modifyPattern type pat
                                                Region ->
                                                    Ok (Region ( Init pat, Empty))
                                                Section before after ->
                                                    Ok (Section {before: before, after: after, pattern : pat})
                                                _ ->
                                                    Err "unknown problem during command processing"
                                Region (pat1, pat2) ->
                                    when (pat1, pat2) is
                                        (Init pattern1, Init pattern2 ) ->
                                            when updateRegionPat pattern2 type is
                                                Ok updatedPat ->
                                                    Ok (Region ( Init pattern1, Init updatedPat ))
                                                Err _ ->  Err "unknown problem during pattern to pattern command processing"
                                        (Init pattern, Empty ) ->
                                            when updateRegionPat pattern type is
                                                Ok updatedPat ->
                                                    Ok (Region ( Init updatedPat, Empty ))
                                                Err _ ->  Err "unknown problem during pattern to pattern command processing"
                                        _ ->
                                            when type is
                                                Regex ->
                                                    Ok (Region ( Init (Allow (Regex "")), Empty ))
                                                LogicalAnd ->
                                                    Ok (Region ( Init (Allow (LogicalAnd [])), Empty ))
                                                _ ->
                                                    Err "unknown problem during pattern to pattern command processing"
                                Section {before: before, after: after, pattern : pat} ->
                                    when pat is
                                        Allow (LogicalAnd lst) ->
                                            when lst is
                                                [] -> Err "unknown problem during command processing"
                                                [.. as head, last ] ->
                                                    when modifyPattern type (andTypesConversion last) is
                                                        Ok (Pattern modifiedPat) ->
                                                            when andTypesConversionBack modifiedPat is
                                                                Ok convertPat ->
                                                                    Ok (Section {before: before, after: after, pattern : (Allow (LogicalAnd (List.append head convertPat)))})
                                                                Err _ -> Err "unknown problem during command processing"
                                                        Err message -> Err message
                                        _ ->
                                            when type is
                                                Regex | LogicalAnd ->
                                                    when modifyPattern type pat is
                                                        Ok (Pattern modifiedPat) ->
                                                            Ok (Section {before: before, after: after, pattern : modifiedPat})
                                                        Err message ->
                                                            Err message
                                                _ -> Err "unknown problem during command processing"
                                Modifier _ ->
                                    Err "unknown problem during command processing"
                                Limit _ ->
                                    Err "unknown problem during command processing"))
                        |> (\ processedResult ->
                            when processedResult is
                                Ok processed ->
                                    when processed is
                                        (Pattern (Allow (LogicalAnd _))) |
                                        (Pattern (Blacklist (LogicalAnd _))) |
                                        (Section {before: _, after: _, pattern : Allow (LogicalAnd _)}) |
                                        (Region (_, Empty)) |
                                        (Region (_, Init (Allow (LogicalAnd _)))) ->
                                            Continue (Ok ( List.dropFirst current.0 1, processed ))
                                        _ ->
                                            Break (Ok ( List.dropFirst current.0 1, processed ))
                                Err message -> Break (Err message)
                            ))
                Err message -> Break (Err message)
            ))
        |> (\ processedResult ->
            when processedResult is
                Ok processed ->
                    (when processed.1 is
                        None ->
                            Err "something is not implemented "
                        Region (pat1, pat2) ->
                            when (pat1, pat2) is
                                (Init pattern1, Init pattern2) ->
                                    when config.command is
                                        None |  Search ->
                                            Ok {config & command : FromPatternToPattern [(pattern1, pattern2)]}
                                        FromPatternToPattern regPat ->
                                            Ok {config & command : FromPatternToPattern (List.append  regPat (pattern1, pattern2))}
                                        _ ->
                                            Err "can't use command like that"
                                _ ->
                                    Err "can't use command like that"
                        Pattern pat ->
                            if config.command == None then
                                Ok {config & command : Search, patterns : List.append config.patterns pat}
                            else
                                Ok {config & patterns : List.append config.patterns pat}
                        Section section ->
                            when config.command is
                                None |  Search ->
                                    Ok {config & command : SearchSection [section]}
                                SearchSection lst ->
                                    Ok {config & command : SearchSection (List.append lst section)}
                                _ ->
                                    Err "can't use commands likethat"
                        Modifier modif ->
                            Ok {config & modifiers : Set.insert config.modifiers modif}
                        Limit lst ->
                            Ok {config & limit : List.concat config.limit lst}
                        _ -> Err "not supported command"
                            )
                    |> ( \ updatedConfigResult ->
                        when updatedConfigResult is
                            Ok upConfig ->
                                updateConfig upConfig processed.0
                            Err message -> Err message)

                Err message -> Err message
        )


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

andUpdate : PatternType -> Result PatternType Str
andUpdate = \ pattern ->
    when pattern is
        Allow (Plain _) ->
            Ok (Allow (LogicalAnd []))
        Color (Plain _) ->
            Err "unsupported option"
        Blacklist (Plain _) ->
            Ok (Blacklist (LogicalAnd []))
        _ -> Err "unsupported option"

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
                                        Err _ ->  result
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
                                            fileLst  = (Utils.filterEmpty ( Utils.tokenizeNewLine file ) )
                                            linesStr = Num.toStr (List.len fileLst)
                                            State.setCommandOutput state  "Enter file analyze mode\n\rEnter filters and execute search\n\rLoaded \(linesStr) lines"
                                            |> State.setFile fileLst
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

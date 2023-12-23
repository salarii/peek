app "command"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br"
    }
    #exposes []
    imports [
        pf.Stdout,
        pf.Stdin,
        Utils,
        Regex,
        Regex.{ParsingResultType},
    ]
    provides [main] to pf


PatternType : [ Regex [Allow Str,Color Str, Blacklist Str], Allow Str, Blacklist Str, Color Str  ]

ModifiersType  : [ NumberLines]

CommandType : [
    Search,
    SearchSection U32 U32 PatternType,
    FromLineToLine  I32 I32,
    FromPatternToPattern PatternType PatternType,
]

AnalyseConfigType :
    { command: CommandType, modifiers : Set ModifiersType, patterns : List PatternType  }


dummyFun = \  parsResult, config ->
    Ok config

colorTag : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
colorTag = \  parsResult, config ->
    { command: com, modifiers : mod, patterns : lst } = config
    when (com, lst) is
        (Search, [Allow pat]) ->
            Ok { config & patterns : [Color pat ] }
        (Search, [Regex (Allow pat)]) -> 
            Ok { config & patterns : [Regex (Color pat) ] }
        _ -> Ok config

regex : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
regex = \  parsResult, config ->
    { command: com, modifiers : mod, patterns : lst } = config
    when (com, lst) is
        (Search, [Allow pat]) ->
            Ok { config & patterns : [Regex (Allow pat) ] }
        (Search, [Color pat]) -> 
            Ok { config & patterns : [Regex (Color pat) ] }
        (Search, [Blacklist pat]) -> 
            Ok { config & patterns : [Regex (Blacklist pat) ] }
        (SearchSection head tail (Allow pat), []) ->
            Ok { config & command: SearchSection head tail (Regex (Allow pat) )}
        _ -> Ok config

createSection : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
createSection = \  parsResult, config ->
    { command: com, modifiers : mod, patterns : lst } = config
    when (com, lst ) is
        (Search, [pattern ]) ->
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
                                                command:
                                                    SearchSection
                                                        (Utils.asciiArrayToNumber arg1 Str.toU32)
                                                        (Utils.asciiArrayToNumber arg1 Str.toU32)  
                                                        pattern,
                                                modifiers : Set.empty {},
                                                patterns : [], 
                                                }
                                        [ 60 ] ->
                                            Ok {
                                                command:
                                                    SearchSection
                                                        (Utils.asciiArrayToNumber arg1 Str.toU32)
                                                        0  
                                                        pattern,
                                                modifiers : Set.empty {},
                                                patterns : [], 
                                                }
                                        [ 62 ] -> 
                                            Ok {
                                                command:
                                                    SearchSection
                                                        0
                                                        (Utils.asciiArrayToNumber arg1 Str.toU32) 
                                                        pattern,
                                                modifiers : Set.empty {},
                                                patterns : [], 
                                                }
                                        _ -> Err "wrong syntax"                           
                                Err message ->  Err message 
                        _ -> Err "wrong syntax" 
        _ -> Err "you try to put to many command " 


createNumberLines : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
createNumberLines = \  parsResult, config ->
    Ok { config & modifiers : Set.insert config.modifiers NumberLines }
    

createBlackListed : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
createBlackListed = \  parsResult, config ->
    { command: com, modifiers : mod, patterns : lst } = config
    when (com, lst ) is
        (Search, [ Allow pat ])  ->
                Ok { config & patterns : [Blacklist pat] }
        (Search, [Regex ( Allow pat ) ])  ->
                Ok { config & patterns : [Regex ( Blacklist pat)] }
        _ -> Ok  config


createPatternToPattern : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
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
        
        
createLineToLine : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
createLineToLine = \  parsResult, config ->
    { command: com, modifiers : mod, patterns : lst } = config
    when com is
        Search  ->  
            when ( 
                Regex.getValue [0] 0 parsResult.captured,
                Regex.getValue [1] 0 parsResult.captured)  is
                (Ok pat1,Ok pat2) ->
                    when (pat1, pat2) is 
                        (['s'],['e'])->
                            Ok { config & command : FromLineToLine  0 -1 }
                        (['s'],val)->
                            Ok { config & command : FromLineToLine  0 (Utils.asciiArrayToNumber val Str.toI32) }
                        (val,['e'])->
                            Ok { config & command : FromLineToLine  (Utils.asciiArrayToNumber val Str.toI32) -1 }
                        (valStart,valEnd)->
                            Ok { config & command : FromLineToLine  (Utils.asciiArrayToNumber valStart Str.toI32)  (Utils.asciiArrayToNumber valEnd Str.toI32) }
                        _ ->
                            Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"
                _ -> Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"
        _ -> Ok config
    
handleOthers : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
handleOthers = \  parsResult, config ->
    when (Regex.getValue [0, 0] 0 parsResult.captured,
        Regex.getValue [0, 1] 0 parsResult.captured ) is
        (Ok modifiers, Ok pattern) ->
            # bit arbitrary and messy but consider wrong command just pattern, (later meybe change  this to show worning)
            if List.isEmpty modifiers == Bool.true then
                Ok { config & patterns : List.append config.patterns (Allow (Utils.utfToStr pattern)) }
            else if List.isEmpty pattern == Bool.true then
                Ok { config & patterns : List.append config.patterns (Allow (Utils.utfToStr modifiers)) }
            else
                modifierAnalysis (Utils.utfToStr modifiers) { command : Search, modifiers : Set.empty {},patterns : [Allow (Utils.utfToStr pattern)] }
                |> ( \ partialConfigResult ->
                    when partialConfigResult is 
                        Ok partialConfig ->
                            { command: com, modifiers : mod, patterns : lst } = config
                            when com is
                                Search -> Ok {command:  partialConfig.command, modifiers : mod, patterns :  List.concat lst partialConfig.patterns}
                                _ -> Ok {config  & patterns : List.concat lst partialConfig.patterns}
                        Err message -> Err message )
                |> ( \ updatedConfigResult  ->  
                    merged = 
                            modifiers
                            |> List.concat pattern
                            |> Utils.utfToStr
                    when updatedConfigResult is
                        Ok updatedConfig -> 
                            { command: com, modifiers : mod, patterns : lst } = updatedConfig
                            when (com, lst) is 
                                (Search, [Allow pat ]) ->
                                    if (Utils.utfToStr pattern) == pat && Set.isEmpty mod == Bool.true then 
                                        Ok { command : Search, modifiers : Set.empty {},patterns : [Allow merged] }
                                    else
                                        updatedConfigResult   
                                _ -> updatedConfigResult
                        Err message -> 
                            Ok { command : Search, modifiers : Set.empty {},patterns : [Allow merged] } )

        _ -> Err "Error when processing \(Utils.utfToStr parsResult.matched) comand"



modifiersHandlers : Dict Str ( ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str)
modifiersHandlers =
    Dict.empty {}
    |> Dict.insert "^[cC]" colorTag
    |> Dict.insert "^[rR]" regex
    |> Dict.insert "^(\\^)(\\d+)" createSection
    |> Dict.insert "^(>)(\\d+)" createSection
    |> Dict.insert "^(<)(\\d+)" createSection
    |> Dict.insert "^[bB]" createBlackListed


modifierAnalysis : Str, AnalyseConfigType -> Result AnalyseConfigType Str
modifierAnalysis = \ word, inState -> 
    if Str.isEmpty  word then 
        Ok inState
    else  
        Dict.keys modifiersHandlers
        |> List.walkUntil (Ok inState) (\ state, pattern ->
            when state  is 
                Ok config ->
                        when Regex.parseStr word pattern is 
                            Ok parsed ->
                                if parsed.result == Bool.true then
                                    when Dict.get modifiersHandlers pattern  is
                                        Ok handler ->
                                            modifiedConfResult = handler parsed config
                                            when modifiedConfResult is
                                                Ok modifiedConf -> Continue (modifierAnalysis (Utils.utfToStr parsed.left) modifiedConf)
                                                Err _ -> Break (Err "internal application error, during comand \(word) analysis")
                                        Err _ -> 
                                            Break (Err "internal application error, during comand \(word) analysis")
                                else 
                                    Continue state
                            Err message -> Break (Err message)
                    
                Err message -> Break (Err message)
        )


commandsToHandlers : Dict Str ( ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str)
commandsToHandlers =
    Dict.empty {}
    |> Dict.insert "^[Nn][Ll]@" createNumberLines
    |> Dict.insert "([Rr])?@(.+)->([Rr])?@(.+)" createPatternToPattern
    |> Dict.insert "^(\\d+|s)->(\\d+|e)@$" createLineToLine
    |> Dict.insert "(([^@]+@)?(.*))" handleOthers
    
simplifiedSyntax : Dict Str Str
simplifiedSyntax = 
    Dict.empty {}

    
commandAnalysis : Str, AnalyseConfigType -> Result AnalyseConfigType Str
commandAnalysis = \ word, inState -> 
    Dict.keys commandsToHandlers
    |> List.walkUntil (Ok inState) (\ state, pattern ->
        when state  is 
            Ok config -> 
                when Regex.parseStr word pattern is 
                    Ok parsed -> 
                        if parsed.result == Bool.true then
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

recoverConfigFromInput : Str -> Result AnalyseConfigType Str
recoverConfigFromInput = \filterStr ->
    Utils.tokenize filterStr
    |> List.walkUntil (Ok { command: Search, modifiers : Set.empty {}, patterns : [] }) (\ state, word ->
    # check  is command 
    # check is simplified command
    #  full command
        when state is 
            Ok config -> 
                when commandAnalysis word config is 
                    Ok updatedConfig -> Continue (Ok updatedConfig )
                    Err message ->  Break (Err message)
            Err message -> Break (Err message)
            ) 

main = 

    dbg  recoverConfigFromInput  "  white "  
    Stdout.write  "Ok"

# rudimentary  tests
expect
    when recoverConfigFromInput  "  white "    is 
        Ok config ->
            config.patterns == [ Allow "white" ] && 
            Set.isEmpty config.modifiers == Bool.true  &&  
            config.command == Search     
        Err mes -> mes == "test search pattern"
        
expect
    when recoverConfigFromInput  "  b@white "    is 
        Ok config ->
            config.patterns == [ Blacklist "white" ] && 
            Set.isEmpty config.modifiers == Bool.true  &&  
            config.command == Search     
        Err mes -> mes == "test search blacklist pattern"


expect
    when recoverConfigFromInput  "  bR@^@[6-7]white "    is 
        Ok config ->
            config.patterns == [ Regex (Blacklist "^@[6-7]white") ] && 
            Set.isEmpty config.modifiers == Bool.true  &&  
            config.command == Search     
        Err mes -> mes == "test search regex pattern"


expect
    when recoverConfigFromInput  " white Nl@ "    is 
        Ok config ->
            dest = 
                Set.empty {}
                |> Set.insert NumberLines

            config.patterns == [ Allow "white" ] && 
            config.modifiers == dest &&  
            config.command == Search     
        Err mes -> mes == "test modifier number lines"


expect
    when recoverConfigFromInput  "  fsflN@ "    is 
        Ok config ->
            config.patterns == [ Allow "fsflN@" ]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == Search     
        Err mes -> mes == "test invalid command, empty pattern"

expect
    when recoverConfigFromInput  "  fsflN@tt "    is 
        Ok config ->
            config.patterns == [ Allow "fsflN@tt" ]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == Search     
        Err mes -> mes == "test invalid command"

expect
    when recoverConfigFromInput  " r@osa->@kosa white"    is 
        Ok config ->
            config.patterns == [Allow "white"]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == FromPatternToPattern (Regex (Allow "osa")) (Allow "kosa")     
        Err mes -> mes == "test from pattern to pattern"

expect
    when recoverConfigFromInput  " s->1000@  white"    is 
        Ok config ->
            config.patterns == [Allow "white"]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == FromLineToLine 0 1000    
        Err mes -> mes == "test from start to 1000 "

expect
    when recoverConfigFromInput  " 100->e@  white"    is 
        Ok config ->
            config.patterns == [Allow "white"]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == FromLineToLine 100 -1   
        Err mes -> mes == "test from 100 to e "

expect
    when recoverConfigFromInput  " ^10r@black[1-9]"    is 
        Ok config ->
            config.patterns == []  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == SearchSection 10 10 (Regex (Allow "black[1-9]"))
        Err mes -> mes == "test region "

expect
    when recoverConfigFromInput  " <10@black"    is 
        Ok config ->
            config.patterns == []  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == SearchSection 10 0 (Allow "black")
        Err mes -> mes == "test region before "

# maybe create more test in the future, at least to cover detected bugs 
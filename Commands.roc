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

# first  cut
PatternType : [ Regex [Allow Str,Color Str], Allow Str, Blacklist Str, Color Str  ]

#availableRegex : Result ( List  TokenPerRegexType ) Str
#availableRegex = Regex.stagesCreationRegex [] 


CommandType : [
    NumberLines,
    SearchSection U32 U32 PatternType,
    FromLineToLine  U32 U32,
    FromPatternToPattern PatternType PatternType,
]

AnalyseConfigType :
    { commands: List CommandType, patterns : List PatternType  }


dummyFun = \  parsResult, config ->
    Ok config

colorTag : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
colorTag = \  parsResult, config ->
    { commands: com, patterns : lst } = config
    when (com, lst) is
        ([], [Allow pat]) ->
            Ok { config & patterns : [Color pat ] }
        ([], [Regex (Allow pat)]) -> 
            Ok { config & patterns : [Regex (Color pat) ] }
        _ -> Ok config

regex : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
regex = \  parsResult, config ->
    { commands: com, patterns : lst } = config
    when (com, lst) is
        ([], [Allow pat]) ->
            Ok { config & patterns : [Regex (Allow pat) ] }
        ([], [Color pat]) -> 
            Ok { config & patterns : [Regex (Color pat) ] }
        ([SearchSection head tail (Allow pat)], []) ->
            Ok { config & commands: [SearchSection head tail (Regex (Allow pat) )]}
            
        _ -> Ok config

createSection : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
createSection = \  parsResult, config ->
    dbg parsResult.captured
    { commands: com, patterns : lst } = config
    when (com, lst ) is
        ([], [pattern ]) ->
            when pattern is 
                Color _ -> Err "don't mix color and commands"
                _ ->        
                    when Regex.getValue [0] 0 parsResult.captured is 
                        Ok operation ->
                            arg1Result = Regex.getValue [1] 0 parsResult.captured  
                            when arg1Result  is 
                                Ok  arg1 -> 
                                    when operation is 
                                        [ 94 ] -> 
                                            Ok {
                                                commands:
                                                    [SearchSection
                                                        (Utils.asciiArrayToNumber arg1 Str.toU32)
                                                        (Utils.asciiArrayToNumber arg1 Str.toU32)  
                                                        pattern]
                                                ,patterns : [] 
                                                }
                                        [ 62 ] ->
                                            Ok {
                                                commands:
                                                    [SearchSection
                                                        (Utils.asciiArrayToNumber arg1 Str.toU32)
                                                        0  
                                                        pattern]
                                                ,patterns : [] 
                                                }
                                        [ 60 ] -> 
                                            Ok {
                                                commands:
                                                    [SearchSection
                                                        0
                                                        (Utils.asciiArrayToNumber arg1 Str.toU32) 
                                                        pattern]
                                                ,patterns : [] 
                                                }
                                        _ -> Err "wrong syntax"                           
                                Err message ->  Err message 
                        _ -> Err "wrong syntax" 
        _ -> Err "you try to put to many commands " 

            
#            
#        Err message -> Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"



createNumberLines : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
createNumberLines = \  parsResult, config -> 
    Ok { config & commands : List.append config.commands NumberLines }

createBlackList : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
createBlackList = \  parsResult, config ->
    when (Regex.getValue [0] 0 parsResult.captured) is
        Ok value -> 
            Ok { config & patterns : List.append config.patterns (Blacklist (Utils.utfToStr value)) }
        Err message -> Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"


createPatternToPattern : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
createPatternToPattern = \  parsResult, config ->
    when ( 
        Regex.getValue [0] 0 parsResult.captured,
        Regex.getValue [1] 0 parsResult.captured,
        Regex.getValue [2] 0 parsResult.captured,
        Regex.getValue [3] 0 parsResult.captured)  is
        (Ok pat1,Ok pat2, Ok pat3, Ok pat4) ->
            when (pat1, pat2, pat3, pat4) is 
                ([_,..],[_,..],[_,..],[_,..])->
                    Ok { config & commands : List.append config.commands (FromPatternToPattern (Regex (Allow (Utils.utfToStr pat2))) (Regex (Allow (Utils.utfToStr pat4) )) ) }
                ([],[_,..],[_,..],[_,..])->
                    Ok { config & commands : List.append config.commands (FromPatternToPattern (Allow (Utils.utfToStr pat2)) (Regex (Allow (Utils.utfToStr pat4))) ) }
                ([_,..],[_,..],[],[_,..])->
                    Ok { config & commands : List.append config.commands  (FromPatternToPattern (Regex (Allow (Utils.utfToStr pat2))) (Allow (Utils.utfToStr pat4)) ) }
                ([],[_,..],[],[_,..])->
                    Ok { config & commands : List.append config.commands (FromPatternToPattern (Allow (Utils.utfToStr pat2)) (Allow (Utils.utfToStr pat4))) }
                _ ->
                    Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"
        _ -> Err "Internal application error when processing \(Utils.utfToStr parsResult.matched) comand"

handleOthers : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
handleOthers = \  parsResult, config ->
    when (Regex.getValue [0, 0] 0 parsResult.captured,
        Regex.getValue [0, 1] 0 parsResult.captured ) is
        (Ok modifiers, Ok pattern) -> 
            if List.isEmpty modifiers == Bool.true then
                Ok { config & patterns : List.append config.patterns (Allow (Utils.utfToStr pattern)) }
            else
                modifierAnalysis (Utils.utfToStr modifiers) { commands : [], patterns : [Allow (Utils.utfToStr pattern)] }
                |> ( \ partialConfigResult ->
                    when partialConfigResult is 
                        Ok partialConfig ->
                            { commands: com, patterns : lst } = config
                            when com is
                                [] | [NumberLines] -> Ok {commands: List.concat com partialConfig.commands, patterns :  List.concat lst partialConfig.patterns}
                                _ -> Ok {config  & patterns : List.concat lst partialConfig.patterns}
                        Err message -> Err message
    )

        _ -> Err "Error when processing \(Utils.utfToStr parsResult.matched) comand"



modifiersHandlers : Dict Str ( ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str)
modifiersHandlers =
    Dict.empty {}
    |> Dict.insert "^[cC]" colorTag
    |> Dict.insert "^[rR]" regex
    |> Dict.insert "^(\\^)(\\d+)" createSection
    |> Dict.insert "^(>)(\\d+)" createSection
    |> Dict.insert "^(<)(\\d+)" createSection
    |> Dict.insert "^@$" dummyFun#beforeSection


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
    #|> Dict.insert "^(\\d+)->(\\d+)@$" "from  line to line"
    |> Dict.insert "(([^@]+@)?(.*))" handleOthers

simplifiedSyntax : Dict Str Str
simplifiedSyntax = 
    Dict.empty {}


# remove  this 
#checkIsCommand : Str -> Result Bool Str
#checkIsCommand = \ word ->
#    isCommandPatterns = 
#        []
#        |> List.append "^([cC])@.+"

#    List.walkUntil isCommandPatterns (Ok Bool.false) (\ state, pattern -> 
        
#        when (Regex.parseStr word pattern, Ok ) is 
#            Ok parsed ->
#                if parsed.result == Bool.true then 
#                    Break  (Ok Bool.true)
#                else 
#                    Continue state
#            Err message -> Break (Err message)
#    )
     

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
                                    Break (Err "internal application error, during comand \(word) analysis")
                        else 
                            Continue state
                    Err message -> Break (Err message)
            Err message -> Break (Err message)
        )

recoverConfigFromInput : Str -> Result AnalyseConfigType Str
recoverConfigFromInput = \filterStr ->
    Utils.tokenize filterStr
    |> List.walkUntil (Ok { commands: [], patterns : [] }) (\ state, word ->
    # check  is command 
    # check is simplified command
    #  full commands
        when state is 
            Ok config -> 
                when commandAnalysis word config is 
                    Ok updatedConfig -> Continue (Ok updatedConfig )
                    Err message ->  Break (Err message)
            Err message -> Break (Err message)
            ) 
tryk  =  \ i ->
    if i == 0 then
        0
    else 
        dbg  recoverConfigFromInput  "^10r@pattern  r@osa->@kosa NL@"    
        tryk  (i - 1)


main = 
    #Stdout.write  clearScreenPat |> Task.await
    
    dbg  tryk  1
    
    Stdout.write  "Ok"

#Str.toUtf8 "c@pattern"



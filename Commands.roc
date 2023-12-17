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
PatternType : [ Regex  Str, Positive Str, Negative Str, Color Str  ]


CommandType : [
    NumberLines,
    SearchSection U32 U32 PatternType,
    FromLineToLine  U32 U32,
    FromPatternToPattern PatternType PatternType,
]

AnalyseConfigType :
    { commands: List CommandType, patterns : List PatternType  }


dummyFun = \  parsResult, config ->
    dbg parsResult.captured
    dbg "here"
    Ok { commands: [], patterns : []  }

createColor : ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str
createColor = \  parsResult, config ->
    dbg Utils.utfToStr parsResult.matched
    when (Regex.getValue [0] 0 parsResult.captured) is
        Ok value -> 
            Ok { config & patterns : List.append config.patterns (Color (Utils.utfToStr value)) }
        Err message -> Err "Internal application message when analysing command"

#when (,

frontCommandsToHandlers : Dict Str ( ParsingResultType, AnalyseConfigType -> Result AnalyseConfigType Str)
frontCommandsToHandlers =
    Dict.empty {}
    #|> Dict.insert "^[Nn][Ll]"  "numberlinesdummy"
    #|> Dict.insert "@(.+)->@(.+)" "patterntopattern"
    #|> Dict.insert "^(\\d+)->(\\d+)@$" "from  line to line"
    #|> Dict.insert "^\\^(\\d+)" "section"
    #|> Dict.insert "^@$" "negative"
    |> Dict.insert "^[cC]@(.+)" createColor

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
    Dict.keys frontCommandsToHandlers
    |> List.walkUntil (Ok inState) (\ state, pattern ->
        when state  is 
            Ok config -> 
                dbg  word
                dbg  pattern
                when Regex.parseStr word pattern is 
                    Ok parsed ->
                        if parsed.result == Bool.true then

                            when Dict.get frontCommandsToHandlers pattern  is
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

main = 
    #Stdout.write  clearScreenPat |> Task.await
    dbg  recoverConfigFromInput  "c@pattern"
    
    Stdout.write  "Ok"

#Str.toUtf8 "c@pattern"



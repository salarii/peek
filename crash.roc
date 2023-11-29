    app "reg"

    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

# I highly prioritized implementation easiness over performance
# this is questionable provided that peek may use matching in extensive way 
# This choice saves mental effort and makes this work possible to be completed 
# I plan to add other means, to enable peek user to be relatively effective time wise anyway 

# I was not interested to recreate regex with all it potential and pitfalls 
# what I want is to make it work for 99% use cases( real life scenarios )


# side note:
# dbg crashing left and right on many complex objects
# I need to run it after some time, with many dbg commands put all over the place,
# to verify improvements

firstStagePatterns = [
        { tag : Dot, str : "."},
        { tag : CaptureOpen, str : "("},
        { tag : CaptureClose, str : ")"},
        { tag : AtLeastOne, str : "+" },
        { tag : ZeroOrMore, str : "*" },
        { tag : Optional, str : "?" },
        { tag : Separator, str : "|" },
        { tag : Digit, str : "\\d" },
        { tag : NoDigit, str : "\\D" },
        { tag : Alphanumeric, str : "\\w" },
        { tag : NoAlphanumeric, str : "\\W" },
        { tag : Whitespace, str : "\\s" },
        { tag : NoWhitespace, str : "\\S" }]
        
# second  stage  patterns
# temporary Only
aux1Patt =  "T(((.+)))T"


secondStagePatterns =  [
    { tag : BackSlash, str : "\\T.?{}^*T" },  # " 
    { tag : Repetition, str : "{(\\d)+}"},   #"   
    { tag : RangeRepetition, str : "{(\\d)+,(\\d)+}" }, # "
    { tag : Only, str : "[(((.)-(.))|((.))|((\\T.?{}^*T)))+]" }, # "
    { tag : Except, str : "[^(((.)-(.))|((.+))|((\\T.?{}^*T)))+]" }]  # "
        

priorities =
    Dict.empty {}
    |> Dict.insert Empty -1
    |> Dict.insert Character 0
    |> Dict.insert Dot 1
    |> Dict.insert CaptureOpen 1
    |> Dict.insert CaptureClose 1
    |> Dict.insert AtLeastOne 1
    |> Dict.insert ZeroOrMore 1
    |> Dict.insert Separator 1
    |> Dict.insert Digit 1
    |> Dict.insert NoDigit 1
    |> Dict.insert Optional 1
    |> Dict.insert Alphanumeric 1
    |> Dict.insert NoAlphanumeric 1
    |> Dict.insert Whitespace 1
    |> Dict.insert NoWhitespace 1
    |> Dict.insert Only 2
    |> Dict.insert BackSlash 2
    |> Dict.insert Repetition 2   
    |> Dict.insert RangeRepetition 2
    
charToUtf = \ char ->
    Str.toUtf8 char
    |> List.first
    |> ( \ res ->
        when res is 
            Ok utf8 -> utf8
            Err _ -> 0  )

emptyNode = { locked : Bool.false, children :  [], value : [] }

treeBase = 
    {cnt : 0, content : Dict.empty {} }
    
createParsingRecord = \ regex, meta ->
    # workaround: I haven't predicted that this information will be needed.
    # on earlier stages it would be cleaner (maybe)to extract this but not with this design
    # I just missed that opportunity and now I don't want to rip through entire thing to make it right

    
    { regex : regex, current : regex, matched : [], result : Bool.false, missed : [], left : [] , captured : treeBase, meta : meta, strict : No } 


checkMatchingValidity = \ matchingResult ->
    when matchingResult.strict is 
        No -> Bool.true 
        Front -> List.isEmpty  matchingResult.missed
        Back -> List.isEmpty  matchingResult.left 
        Both -> (List.isEmpty  matchingResult.missed) && (List.isEmpty  matchingResult.left)

modifyLastInList = \ lst, elem ->
    List.dropLast lst 1
    |> List.append elem



createToken = \ token, serie, capture ->
    { token :token, serie : serie, capture : capture  }

regexSeedPattern = [
    { tag : Character, tokens : [ createToken  Dot  Once Bool.false ] } ]


    

checkMatching = \ utfLst, reg  ->
    
    matchStr = \ utf, pattern ->
        Consume
    
    matchUtf = ( \ utf, tokenMeta ->
        #dbg "check  matching"
        #dbg  Str.fromUtf8 [utf]
        #dbg printMe [tokenMeta]
        result = matchStr utf tokenMeta.token
       
        when result is
            Consume -> Consume tokenMeta 
            NoMatch -> NoMatch tokenMeta 
    )

    updateRegex = (\regex ->  
        updateRegexItemInternal = ( \ state, regItem ->
                
                when List.first regItem.current is 
                    Ok pat ->
                        when pat.token  is
                            Sequence  chain ->
                                when pat.serie is 
                                    _ ->
                                        List.concat chain (List.dropFirst regItem.current 1)
                                        |> (\ updatedCurrent ->  
                                            List.append state regItem  )
                            _ -> 
                                when pat.serie is 
                                    Once ->
                                        List.append state regItem 
                                    _ ->
                                        List.append state regItem
                    Err _ -> List.append state regItem )
     
        List.walk  regex [] ( \ state, regItem ->
            updateRegexItemInternal state regItem
            )
        )

    getFirstPat = (\  state  ->  
        if state.meta == Inactive then
            Inactive state
        else
            when List.first state.current is
                Ok pat -> 
                    Active { pattern : pat , state : state } 
                Err _ ->  
                    Inactive {state & meta : Inactive } 
            )

    regeneratePat = (\  state  -> 
        when List.first state.current is
            Ok pat -> 
                Old state
            Err _ ->  
                if state.meta == Origin then
                    # BUG  in this  line
                    # getFirstPat  { state & current : state.regex } 
                    New { regex : state.regex, current : state.regex, matched : state.matched, result : state.result, missed : state.missed, left : state.left, captured : state.captured, meta : state.meta, strict : state.strict } 
                else 
                    New {state & meta : Inactive }  )
            
            
    checkLastListEmpty = (\ listOfLists  ->
            when List.last listOfLists is
                Ok lst -> 
                    List.isEmpty lst
                Err _ ->
                    Bool.false  
    
        )

    complexSearch = 
        List.walk utfLst [createParsingRecord reg Origin]  ( \ outState, utf ->
            
            updatedStates = updateRegex outState 
            List.walk updatedStates [] ( \ state, processedReg ->   

                if processedReg.result == Bool.true then
                    List.append state { processedReg & left : List.append  processedReg.left utf} 
                else   
                    manageIteration = ( \ inProcessedReg,curState ->
                  
                        when getFirstPat inProcessedReg is
                            Inactive patternSet ->
                                List.append curState inProcessedReg
                            Active matchThis ->                                    
                                    updatedState = matchThis.state
  
                                    current = List.dropFirst  updatedState.current  1
                                    #   BUG  this  crashes 
                                    #ppp = List.isEmpty current == Bool.true 
                                    updatedCapture =     updatedState.captured
                                    when matchUtf utf  matchThis.pattern is 
                                        Consume updatedToken ->
                                            
                                            allowEnd = \ lst, allowInState ->
                                                List.walkUntil lst {allow: Bool.true, state : allowInState }  ( \allowState, elem ->
                                                    if elem.token == CaptureClose then 
                                                        allowStateUpdate = allowState.state
                                                        Continue {allowState & state : { allowStateUpdate &   captured : allowStateUpdate.captured } }
                                                        
                                                    else
                                                        Break  { allowState & allow :  Bool.false } 
                                                ) 
                                            result = allowEnd current updatedState
                                            if result.allow == Bool.true then
                                                allowState = result.state
                                                List.append curState { allowState &  matched : List.append allowState.matched  utf, current : [], result : Bool.true, left : [], captured : updatedCapture}
                                            else
                                                when regeneratePat updatedState is 
                                                    Old _ -> 
                                                        List.append curState { updatedState & matched : List.append updatedState.matched  utf, current : current, left : [], captured : updatedCapture} 
                                                    New newState ->  List.append curState  newState
                                        NoMatch _ ->

                                                updateMissed = 
                                                    List.concat updatedState.missed  updatedState.matched
                                                    |> List.append  utf

                                                # BUG this line 
                                                #List.append curState { updatedState &  matched : [], current : updatedState.regex, missed : List.append updateMissed utf, left : [], captured : treeBase}
                                                List.append curState { regex : updatedState.regex, current : updatedState.regex, matched : [], result : updatedState.result, missed : updateMissed, left : updatedState.left, captured : treeBase, meta : updatedState.meta, strict : updatedState.strict }
                                                
                                        _ -> curState  
                                )
                    manageIteration processedReg state ) 
        )
    List.walk complexSearch  (createParsingRecord reg Inactive) ( \ state, parsResult -> 

        if state.result == Bool.true then
            if (List.len parsResult.matched > List.len state.matched ) && (checkMatchingValidity state) then 
                parsResult
            else 
                state
        else 
            parsResult )
    



 
# dbg does not always  works so I  am using this : ) 
printMe = (  \ arrayPrt ->
    List.walk arrayPrt "" (  \ inState , token ->
        when  token.token  is 
            Character  val -> 
                extractedChar = 
                    when Str.fromUtf8 [val] is 
                        Ok str -> str
                        Err _ -> "  some weird problem in  extracting character token "
                
                Str.concat inState "\n"
                |> Str.concat  "character : "
                |> Str.concat  extractedChar  
            Dot ->
                Str.concat inState "\nDot"
            CaptureOpen ->
                Str.concat inState "\nCapture  Open"
            CaptureClose ->
                Str.concat inState "\nCapture  Close"
            Separator ->
                Str.concat inState "\nSeparator"
            Sequence  array  ->    
                Str.concat inState "\nseq : -> " 
                |> Str.concat (printMe array)
                |> Str.concat "\nexit sequence  <- "
            AtLeastOne ->
                Str.concat inState "\nAt least one"
            Once -> Str.concat inState "\nOnce"
            Digit -> Str.concat inState "\nDigit"
            BackSlash -> Str.concat inState "\nBackSlash"
            CharacterSet _ -> Str.concat inState "\nCharacter set"  
            LimitRanges _ -> Str.concat inState "\nLimit Ranges set"
            _ -> 
                "\n unknown token "
        )
)


main =

    rres = checkMatching (Str.toUtf8  "a" )  [(createToken  ( Character 'a' )  Once Bool.false )]
    
    Stdout.line "outStr"



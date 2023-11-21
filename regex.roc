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
aux1Patt =  "[((.+))]"

secondStagePatterns =  [
    { tag : BackSlash, str : "\\[.?{}^*]" },  # " 
    { tag : Repetition, str : "{(\\d)}"},   #"   
    { tag : RangeRepetition, str : "{(\\d),(\\d)}" }, # "
    { tag : Only, str : "[(((.)-(.))|((.+))|(\\[.?{}^*]))+]" }, # "
    { tag : Except, str : "[(((.)-(.))|((.+))|(\\[.?{}^*]))+]" }]  # "
        
#auxiliaryPatterns = 
#    [ tag : CharacterSet, str : ".-."  ]
#    |> List.concat regexSeedPattern
     
     
#secondStagePatterns = [
#    tag : Only,  str : "[.+]"
#    tag : NotThis, str : "[^.+]"
#]     
        
#thirdStagePatterns = [
#    { tag : BackSlash, str : "\([.?{}^\*])" },
#    { tag : Repetition, str : "{(\d)}" },
#    { tag : RangeRepetition, str : "{(\d),(\d)}" }    
#]   


priorities =
    Dict.empty {}
    |> Dict.insert Empty -1
    |> Dict.insert Character 0
    |> Dict.insert Dot 1
    |> Dict.insert CaptureOpen 1
    |> Dict.insert CaptureClose 1
    |> Dict.insert AtLeastOne 1
    |> Dict.insert Separator 1
    |> Dict.insert Digit 1
    |> Dict.insert NoDigit 1
    |> Dict.insert Alphanumeric 1
    |> Dict.insert NoAlphanumeric 1
    |> Dict.insert Whitespace 1
    |> Dict.insert NoWhitespace 1
    |> Dict.insert Only 2
    |> Dict.insert BackSlash 2
    
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
    |> addElement  0 emptyNode
    
createParsingRecord = \ regex, meta ->
    # workaround: I haven't predicted that this information will be needed.
    # on earlier stages it would be cleaner (maybe)to extract this but not with this design
    # I just missed that opportunity and now I don't want to rip through entire thing to make it right
    stricSetting = 
        when (List.first regex, List.last regex ) is
            (Ok  (  tokenFirst) , Ok (  tokenLast) ) ->
                when ( tokenFirst.token, tokenLast.token )  is 
                    (Character  first,Character  last )  ->
                        { updatedRegex : 
                            List.dropFirst regex 1
                            |> List.dropLast 1,
                            strict : Both }
                    (Character  first, _) ->
                        { updatedRegex : List.dropFirst regex 1, strict : Front }
                    (_, Character  last) ->
                        { updatedRegex : List.dropLast regex 1, strict : Back }
                    _ -> 
                        { updatedRegex : regex, strict : No }
            _ ->  { updatedRegex : regex, strict : No }

    { regex : stricSetting.updatedRegex, current : regex, matched : [], result : Bool.false, missed : [], left : [] , captured : treeBase, meta : meta, strict : stricSetting.strict } 


checkMatchingValidity = \ matchingResult ->
    when matchingResult.strict is 
        No -> Bool.true 
        Front -> List.isEmpty  matchingResult.missed
        Back -> List.isEmpty  matchingResult.left 
        Both -> (List.isEmpty  matchingResult.missed) && (List.isEmpty  matchingResult.left)

modifyLastInList = \ lst, elem ->
    List.dropLast lst 1
    |> List.append elem


getPrioToken = \ patterns ->
    if List.isEmpty patterns then
        Err NoTokens
    else
        List.walk patterns (Err Empty) (\ state, pattern -> 
            when state is 
                Err Empty ->
                    when Dict.get priorities  pattern.tag is
                        Ok _ -> Ok pattern
                        Err _ -> Err PriorityListErr 
                Ok prevPat ->  
                          
                    when Dict.get priorities prevPat.tag is
                        Ok val1 ->
                            when Dict.get priorities  pattern.tag is
                                Ok val2 ->
                                    if val2 > val1 then 
                                        Ok pattern
                                    else 
                                        state
                                Err _ -> Err PriorityListErr 
                        Err _ -> Err PriorityListErr
                Err message -> Err message 
        )

createToken = \ token, serie, capture ->
    { token :token, serie : serie, capture : capture  }

regexSeedPattern = [
    { tag : Character, tokens : [ createToken  Dot  Once Bool.false ] } ]

#  test  this, I could not figure  out  how to do this properly 
splitChainOnSeparators = \ chain, inputLst ->
    when List.first chain is 
        Ok elem ->
            when elem.token is 
                Separator -> 

                        List.walk ( splitChainOnSeparators (List.dropFirst chain 1) []) [] ( \ state, lst ->
                            List.append state lst
                        )
                        |>   List.append []
                Sequence  inChain ->

                    partialSeqResult =
                        List.walk ( splitChainOnSeparators inChain []) [] ( \ outState, frontLst ->                               
                            
                            List.walk ( splitChainOnSeparators (List.dropFirst chain 1) []) outState ( \ state, lst ->                                
                                List.append state  ( List.concat [ {elem & token : Sequence frontLst } ]  lst) 
                            )        
                        )   
          
                    when List.last partialSeqResult is
                        Ok activElem ->
                            List.dropLast partialSeqResult 1
                            |> List.walk  [] ( \ state,  lst ->
                                List.append state (List.concat inputLst lst))
                            |> List.append  activElem
                        Err _ ->
                            []
                     
                _ ->

                    partialResult =
                        List.walk ( splitChainOnSeparators (List.dropFirst chain 1) (List.append inputLst elem )) [] ( \ state, lst ->
                                List.append state lst
                        )

                    when List.last partialResult is 
                        Ok lst -> 
                            modifyLastInList partialResult  (List.concat [elem] lst)  
                        Err _ -> 
                            []
                    
        Err _ ->
            [[]]
        

    
evalRegex = \ utfLst, patterns, regex ->
    if List.isEmpty utfLst then
        Ok regex
    else
 
        List.walk patterns [] ( \ state, pattern ->
            out = checkMatching utfLst pattern.tokens
            if out.result == Bool.true  && List.isEmpty out.missed then
                List.append state { tag : pattern.tag, parsedResult : out } 
            else
                state 
            )
        |> getPrioToken
        |> ( \ prioToken ->
            when prioToken is
                Ok token -> evalRegex token.parsedResult.left patterns  ( List.append regex token ) 
                Err message ->  Err message  )

checkMatching = \ utfLst, reg  ->
    
    matchStr = \ utf, pattern ->
        checkRange = ( \ val, ranges -> 
                        List.walk ranges Outside  (\ _state, elem ->
                            if val >= elem.left  && val <= elem.right then
                                Within
                            else
                                Outside ))
        when pattern is 
            LimitRanges lst -> 
                when checkRange utf lst is 
                    Within -> Consume
                    Outside -> NoMatch
            ReverseLimitRanges lst -> 
                when checkRange utf lst is 
                    Within -> NoMatch
                    Outside -> Consume
            CharacterSet lst ->
                when List.findFirst lst  ( \ char -> char == utf ) is 
                    Ok _ -> Consume
                    Err _ -> NoMatch 
            NotInCharacterSet lst ->
                when List.findFirst lst  ( \ char -> char == utf ) is 
                    Ok _ -> NoMatch
                    Err _ -> Consume    
            Digit ->   # fix  that  later
                when checkRange utf [{ left : 48, right : 57 }] is 
                    Within -> Consume
                    Outside -> NoMatch
            NoDigit -> 
                when checkRange utf [{ left : 48, right : 57 }] is 
                    Within -> NoMatch
                    Outside -> Consume
            Except  tokens  ->
                #List.walkUntil tokens Consume ( \  state , token  ->
                #    when matchStr  utf  token is
                #        Consume -> Break  NoMatch
                #        NoMatch -> Continue state 
                #    ) 
                    
                Consume
            Character val ->
                if val == utf then
                    Consume    
                else
                    NoMatch
            Dot ->
                Consume
            _ -> NoMatch
    
    matchUtf = ( \ utf, tokenMeta ->
        result = matchStr utf tokenMeta.token
       
        when result is
            Consume -> Consume tokenMeta 
            NoMatch -> NoMatch tokenMeta 
    )

    updateRegex = (\regex ->  
        updateRegexItemInternal = ( \ state, regItem ->
                
                when List.first regItem.current is 
                    Ok pat ->
                        concatIter = (\ n , lst, stored ->
                            if n == 0 then 
                                stored
                            else 
                                concatIter (n-1) lst (List.concat lst stored  ))
                        when pat.token  is
                            CaptureOpen ->
                                tmpState = {regItem & current : List.dropFirst regItem.current 1}
                                updateRegexItemInternal state { tmpState &  captured : (composeMatchedStructure tmpState.captured  0 CaptureOpen) }  
                            CaptureClose ->        
                                tmpState = {regItem & current : List.dropFirst regItem.current 1}    
                                updateRegexItemInternal state { tmpState &   captured : composeMatchedStructure tmpState.captured  0 CaptureClose } 
                        
                            Sequence  chain ->
                                when pat.serie is 
                                    AtLeastOne ->   
                                        changeFront = 
                                            (List.dropFirst regItem.current 1)
                                            |> List.prepend { pat & serie : ZeroOrMore }
                                        
                                        List.concat chain changeFront
                                        |> (\ updatedCurrent ->  
                                                List.concat state (updateRegexItemInternal [] {regItem & current : updatedCurrent} ))
                                        |> ( \ updatedState ->
                                            List.concat chain (List.dropFirst regItem.current 1)
                                            |> (\ updatedCurrent ->  
                                                    List.concat updatedState (updateRegexItemInternal [] {regItem & current : updatedCurrent}) ))
                                    ZeroOrMore ->
 
                                        List.concat state
                                            (List.concat chain  regItem.current
                                            |> (\ updatedCurrent -> ( updateRegexItemInternal [] {regItem & current : updatedCurrent, meta : Active} )))
                                        |> List.concat  ( updateRegexItemInternal [] {regItem & current : (List.dropFirst regItem.current 1)} )
                                    
                                    MNTimes m n -> 
                                        changeFront = 
                                            (List.dropFirst regItem.current 1)
                                            |> List.prepend { pat & serie : NoMorethan n }
                                            
                                        List.concat (concatIter m chain  [] ) changeFront
                                        |> (\ updatedCurrent -> 
                                                 List.concat state ( updateRegexItemInternal [] {regItem & current : updatedCurrent} ) )

                                    NoMorethan cnt -> 
                                        if  cnt == 0 then
                                            List.concat state  ( updateRegexItemInternal [] {regItem & current : (List.dropFirst regItem.current 1)} )
                                        else 
                                            changeFront = 
                                                (List.dropFirst regItem.current 1)
                                                |> List.prepend { pat & serie : NoMorethan (cnt - 1) }
                                            
                                            List.concat chain changeFront
                                            |> (\ updatedCurrent ->  
                                                List.concat state (updateRegexItemInternal [] {regItem & current : updatedCurrent} ))
                                            |> ( \ updatedState ->
                                                List.concat chain (List.dropFirst regItem.current 1)
                                                |> (\ updatedCurrent ->  
                                                        List.concat updatedState (updateRegexItemInternal [] {regItem & current : updatedCurrent}) ))
                                                
                                    NTimes cnt -> 
                                        
                                        List.concat (concatIter cnt chain  [] ) (List.dropFirst regItem.current 1)
                                        |> (\ updatedCurrent -> 
                                                 List.concat state ( updateRegexItemInternal [] {regItem & current : updatedCurrent} ) )  
                                    Once ->
                                        List.concat chain (List.dropFirst regItem.current 1)
                                        |> (\ updatedCurrent ->  
                                            List.concat state (updateRegexItemInternal [] {regItem & current : updatedCurrent}) )  
                            _ -> 
                                when pat.serie is 
                                    AtLeastOne ->      
                                        changeFront = 
                                            (List.dropFirst regItem.current 1)
                                            |> List.prepend { pat & serie : ZeroOrMore }
                                            
                                        List.concat [{pat & serie : Once}] changeFront
                                        |> (\ updatedCurrent ->  List.append state {regItem & current : updatedCurrent} )  
                                        |> ( \ updatedState ->
                                            List.concat [{pat & serie : Once}] (List.dropFirst regItem.current 1)
                                            |> (\ updatedCurrent ->  List.append updatedState {regItem & current : updatedCurrent} ))
                                    ZeroOrMore ->
                                        List.append state
                                            (List.concat [{pat & serie : Once}]  regItem.current 
                                            |> (\ updatedCurrent ->  {regItem & current : updatedCurrent, meta : Active} ))
                                    
                                        |> List.concat  ( updateRegexItemInternal [] {regItem & current : (List.dropFirst regItem.current 1)} )
                                    
                                    MNTimes m n ->
                                            changeFront = 
                                                (List.dropFirst regItem.current 1)
                                                |> List.prepend { pat & serie : NoMorethan n }
                                                
                                            List.concat (concatIter m [{pat & serie : Once}]  [] ) changeFront
                                            |> (\ updatedCurrent ->  List.append state {regItem & current : updatedCurrent} )
                                    NoMorethan cnt ->
                                        if  cnt == 0 then
                                           List.concat state ( updateRegexItemInternal [] {regItem & current : (List.dropFirst regItem.current 1)} ) 
                                        else 
                                            changeFront = 
                                                (List.dropFirst regItem.current 1)
                                                |> List.prepend { pat & serie : NoMorethan (cnt - 1) }
                                                
                                            List.concat [{pat & serie : Once}] changeFront
                                            |> (\ updatedCurrent ->  List.append state {regItem & current : updatedCurrent} )  
                                            |> ( \ updatedState ->
                                                List.concat [{pat & serie : Once}] (List.dropFirst regItem.current 1)
                                                |> (\ updatedCurrent ->  List.append updatedState {regItem & current : updatedCurrent} ))
                                    NTimes cnt -> 
                                        List.concat (concatIter cnt [{pat & serie : Once}]  [] ) (List.dropFirst regItem.current 1)
                                        |> (\ updatedCurrent ->  List.append state {regItem & current : updatedCurrent} )  
                                    Once ->
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
                    if state.meta == Origin then
                        # BUG  in this  line
                        # getFirstPat  { state & current : state.regex } 
                        getFirstPat  { regex : state.regex, current : state.regex, matched : state.matched, result : state.result, missed : state.missed, left : state.left, captured : state.captured, meta : state.meta, strict : state.strict } 

                    else   
                        Inactive {state & meta : Inactive } 
            )

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
                                    updatedCapture =
                                        if matchThis.pattern.capture == Bool.true then
                                            changeValue updatedState.captured 0 utf  
                                        else
                                            updatedState.captured
                                    when matchUtf utf  matchThis.pattern is 
                                        Consume updatedToken ->
                                             
                                            if List.len current == 0 then
                                                List.append curState { updatedState &  matched : List.append updatedState.matched  utf, current : current, result : Bool.true, left : [], captured : updatedCapture}
                                            else
                                                List.append curState { updatedState & matched : List.append updatedState.matched  utf, current : current, left : [], captured : updatedCapture} 
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
    
getRegexTokens = \ result  -> 
    when result.tag is 
        Character-> 
            when List.first result.parsedResult.matched is 
                Ok  matched  -> 
                    Ok [(createToken  ( Character matched )  Once Bool.false )]
                Err  _  -> Err "character  tag problem"
        _ -> 
            Err "wrong tag"


 

regexCreationStage = \ inPatterns, ignitionPatterns ->

    regPatterns = 
        List.walk inPatterns (Ok []) ( \ state, pat->
            when state is 
                Ok patterns -> 
                    when evalRegex (Str.toUtf8  pat.str ) ignitionPatterns [] is 
                        Ok results ->
                            
                            List.walk  results (Ok []) ( \ inState, result ->
                                when inState is 
                                    Ok patLst ->
                                        when  getRegexTokens result is 
                                            Ok tokens -> 
                                                workaround = 
                                                    List.walk  tokens [] ( \ workState, token -> 
                                                        List.append workState (createToken token.token token.serie  token.capture) )
                                                # !!!!!!!!!!! crashes  here Ok [{ tag : pat.tag, tokens : tokens }]
                                                Ok (List.concat patLst  workaround  )    

                                            Err message -> Err message 
                                    Err  message -> Err message     
                                
                            )
                            |> (\ inTokensResult ->
                                when  inTokensResult is 
                                    Ok inTokens ->  
                                        Ok ( List.append patterns { tag : pat.tag, tokens : inTokens } )
                                    Err message ->  Err message )

                        Err Empty ->  Err "Empty" 
                        Err NoTokens ->  Err "NoTokens"
                        Err PriorityListErr ->  Err "PriorityListErr"
                Err message ->  Err message )
    when regPatterns is 
        Ok patterns ->
            Ok  ( List.concat  ignitionPatterns  patterns )
        Err  message  -> Err  message  
    
    
stagesCreationRegex  = \ _param -> 
    
    # this is not really needed 
    # all patterns could be placed in regexSeedPattern right out of the bat (in token form)
    # but this approach has some benefits:
    # - quite decent self validation during development
    # - additional restrictions which limit unbounded sea of possibilities during design phase
    # - patterns are easy to recognize from their's string form
    stage1 = regexCreationStage firstStagePatterns regexSeedPattern
    when stage1 is 
        Ok stage1Pat ->
            patternAux1Result = regexCreationStage2 aux1Patt stage1Pat  []
            when patternAux1Result is 
                Ok patternAux1 ->
                    stage2Pat = List.append stage1Pat  { tag : Only, tokens : patternAux1 }

                    List.walkUntil  secondStagePatterns (Ok stage1Pat) ( \ state, pat  ->
                        when state is 
                            Ok currentPatterns ->
                                patternResult = regexCreationStage2 pat.str stage2Pat  []


                                when patternResult is 
                                    Ok pattern ->
                                        Continue (Ok ( List.append currentPatterns { tag : pat.tag, tokens : pattern }))
                                    Err message -> Break (Err message)
                            Err message -> Break (Err message)
                    )
                    
                Err message -> Err message   

        Err message -> Err message


regexCreationStage2  = \ str, patterns, currReg ->
    ( evalRegex (Str.toUtf8  str) patterns currReg )
    |> ( \ resultSet -> 
        when resultSet is
            Ok  results ->
                List.walk results (Ok { lst : [] , capture : Bool.false }) ( \ outState, result  ->
                    when outState is 
                        Err message -> Err message 
                        Ok state ->
                            modifLastInChain = (\ chainLst, token ->
                                when List.last chainLst is 
                                    Ok elem ->
                                        when elem.token is 
                                            Sequence  chain ->
                                                when List.last chain is
                                                    Ok lastOnChain ->
                                                        closeLast = (\  lst, seq ->
                                                            Sequence  seqChain = seq  
                                                            when List.last seqChain is
                                                                Ok  item  -> 
                                                                    when item.token is 
                                                                        Sequence  inChain ->
                                                                            modifyLastInList 
                                                                                lst (createToken (Sequence (closeLast seqChain (Sequence  inChain) ))  Once Bool.false)     
                                                                        _ ->
                                                                            List.append lst token 
                                                                Err _ ->     
                                                                        lst
                                                                        )
                                                                    
                                                            
                                                        when token.token is 
                                                            CaptureClose ->
                                                                closeLast chainLst (Sequence chain) 
                                                               # List.append chainLst token            
                                                            _ -> 
                                                                modifyLastInList chainLst (createToken (Sequence ( modifLastInChain chain token))  Once Bool.false)

                                                    Err _ ->
                                                        modifyLastInList  chainLst  (createToken (Sequence ( modifLastInChain chain token))  Once Bool.false)
                                            _ ->  
                                                List.append chainLst token                  
                                    Err _ ->
                                        List.append chainLst token
                                        
                            )
                            # those two functions below exist because of some failures in design
                            createUpdatedToken = ( \ token, cnt, op,  tokens ->
                                        when token is
                                            Ok elem ->
                                                when elem.token is
                                                    Sequence chain ->
                                                        createUpdatedToken  (List.last chain)  cnt op (List.append tokens elem)      
                                                    _ ->
                                                        List.append tokens elem
                                                        |> List.dropLast cnt
                                                        |> List.walkBackwards  (Err "internal error during  + evaluation ") (  \ createResult , elemToken   -> 
                                                            when elemToken.token is
                                                                Sequence chain ->
                                                                    when chain is 
                                                                        [.., elemChain ] ->
                                                                            when createResult is 
                                                                                Ok updatedToken ->
                                                                                    Ok (createToken (Sequence (modifyLastInList chain updatedToken)) elemToken.serie elemToken.capture )
                                                                                _ ->
                                                                                    Ok ( op elemToken )
                                                                        [] -> Err " wrong + usage "
                                                                _->
                                                                    Ok ( op elemToken ))
                                               
                                            Err  _ ->  Err " wrong + usage "
                                    )
                                    
                            omitCaptureEnd = ( \ inLst, op, cnt -> 
                                        when List.last inLst is
                                            Ok elem ->
                                                when elem.token is 
                                                    CaptureClose ->
                                                        when omitCaptureEnd ( List.dropLast inLst  1) op (cnt +1) is 
                                                            Ok updatedLst ->
                                                                Ok (
                                                                    List.append updatedLst elem)
                                                            Err message -> Err message 
                                                    _ ->
                                                        when  createUpdatedToken (Ok elem)  cnt op [] is
                                                            Ok updatedLast ->  Ok ( modifyLastInList inLst updatedLast )
                                                            Err  message -> Err  message  
                                                        
                                                
                                            Err _ -> Err "Wrong usage of + in pattern"        
                                        )           
                            when  result.tag is 
                                Character ->
                                    when List.first result.parsedResult.matched is 
                                        Ok  matched  ->
                                            Ok { state &  lst : modifLastInChain state.lst  (createToken (Character matched) Once state.capture) }
                                        Err _ -> Err "parser  match problem"
                                    
                                Dot ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken Dot Once state.capture )  }
                                CaptureOpen ->
                                    
                                    openLst = 
                                        modifLastInChain state.lst (createToken CaptureOpen Once Bool.false)
                                        |> modifLastInChain  (createToken (Sequence []) Once Bool.false)
                                    Ok { lst : openLst, capture : Bool.true}
                                CaptureClose ->
                                    Ok { lst : modifLastInChain state.lst (createToken CaptureClose Once Bool.false), capture : Bool.false }
                                Separator -> 
                                    Ok { state &  lst : modifLastInChain state.lst (createToken Separator Once Bool.false )  }
                                
                                Digit ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken Digit Once Bool.false )  }
                                NoDigit ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken NoDigit Once Bool.false )  }
                                Alphanumeric ->
                                    limitRangToken = LimitRanges [{ left : 'A', right : 'Z' },{ left : 'a', right : 'z' },{ left : '0', right : '9' }]
                                    Ok { state &  lst : modifLastInChain state.lst (createToken limitRangToken Once Bool.false )  }
                                NoAlphanumeric ->
                                    limitRangToken = ReverseLimitRanges [{ left : 'A', right : 'Z' },{ left : 'a', right : 'z' },{ left : '0', right : '9' }]
                                    Ok { state &  lst : modifLastInChain state.lst (createToken limitRangToken Once Bool.false )  }
                                Whitespace ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken (CharacterSet [0x20, 0x09, 0x0D, 0x0A]) Once Bool.false ) }
                                NoWhitespace ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken (NotInCharacterSet [0x20, 0x09, 0x0D, 0x0A]) Once Bool.false ) }
                                Only -> 
                                    when createOnlyOutOfTree result.parsedResult.captured is
                                        Ok onlyToken -> 
                                            Ok { state &  lst : modifLastInChain state.lst onlyToken }
                                        Err message ->
                                            Err message
                                Except -> 
                                    Ok state
                                BackSlash->
                                    when result.parsedResult.matched is 
                                        [backslash, sign ] ->
                                            Ok { state &  lst : modifLastInChain state.lst  (createToken (Character sign) Once state.capture) }
                                        _ -> Err "back slash parser  match problem"
                                    
                                Repetition ->
                                    when createRepetitionOutOfTree result.parsedResult.captured is
                                        Ok serie -> 
                                            when omitCaptureEnd state.lst ( \ inElem -> {inElem & serie : serie } )  0 is 
                                                Ok newLst ->
                                                    Ok { state &  lst : newLst }
                                                Err message -> Err message     
                                        Err message ->
                                            Err message  
                                RangeRepetition ->
                                    when createRepetitionOutOfTree result.parsedResult.captured is
                                        Ok serie -> 
                                            when omitCaptureEnd state.lst ( \ inElem -> {inElem & serie : serie } )  0 is 
                                                Ok newLst ->
                                                    Ok { state &  lst : newLst }
                                                Err message -> Err message                                              
                                        Err message ->
                                            Err message 
                                ZeroOrMore ->                                            
                                    when omitCaptureEnd state.lst ( \ inElem -> {inElem & serie : ZeroOrMore } )  0 is 
                                        Ok newLst ->
                                            Ok { state &  lst : newLst }
                                        Err message -> Err message  
                                Optional ->
                                     when omitCaptureEnd state.lst ( \ inElem -> {inElem & serie : (NoMorethan 1) } )  0 is 
                                        Ok newLst ->
                                            Ok { state &  lst : newLst }
                                        Err message -> Err message  
                                AtLeastOne ->
                                    

                                    when omitCaptureEnd state.lst ( \ inElem -> {inElem & serie : AtLeastOne } )  0 is 
                                        Ok newLst ->
                                            Ok { state &  lst : newLst }
                                        Err message -> Err message  
                     
                                Empty -> Err "Empty"  
                        )
                |> (\ tokenLst -> 
                    when tokenLst is 
                        Ok lstRec ->
                            Ok lstRec.lst
                        Err message -> Err message  )
                         
            Err Empty ->  Err "Empty" 
            Err NoTokens ->  Err "NoTokens"
            Err PriorityListErr ->  Err "PriorityListErr"
    )
    

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
            Sequence  array  ->    
                Str.concat inState "\nseq : -> " 
                |> Str.concat (printMe array)
                |> Str.concat "\nexit sequence  <- "
            AtLeastOne ->
                Str.concat inState "\nAt least one"
            Once -> Str.concat inState "\nOnce"
            BackSlash -> Str.concat inState "\nBackSlash"
            CharacterSet _ -> Str.concat inState "\nCharacter set"  
            LimitRanges _ -> Str.concat inState "\nLimit Ranges set"
            _ -> 
                "\n unknown token "
        )
        
        
)


addElement = \ tree, parentId ,node -> 
    when Dict.get tree.content parentId  is 
        Ok element -> 
            updatedContent = 
                Dict.remove tree.content parentId
                |> Dict.insert parentId {element  & children : (List.append element.children tree.cnt )}
            
            { cnt : tree.cnt + 1, content : (Dict.insert updatedContent  tree.cnt node )  }          
        Err _ -> { cnt : tree.cnt + 1, content : Dict.insert tree.content  tree.cnt node }
    
    
changeElement = \ tree, id ,node -> 
    when Dict.get tree.content id  is 
        Ok element -> 
            updatedContent = 
                Dict.remove tree.content id
                |> Dict.insert id node
            
            { tree & content : updatedContent }          
        Err _ -> { cnt : tree.cnt + 1, content : Dict.insert tree.content  tree.cnt node }
    

getDirectly = \ tree, id  ->
    when Dict.get tree.content id  is 
        Ok node ->
            Ok node.value  
        Err _ -> Err  "no such value"

getDirectlyChildrenCnt = \ tree, id  ->
    when Dict.get tree.content id  is 
        Ok node ->
            Ok (List.len node.children )  
        Err _ -> Err  "no such node"


getValue = \ indices, parentId, tree ->
     
    when List.first indices is 
        Ok indice -> 
            when Dict.get tree.content parentId  is 
                Ok node ->
                    when List.get node.children indice is 
                        Ok childIdx -> 
                            if List.len indices == 1 then
                                getDirectly tree childIdx
                            else
                                getValue (List.dropFirst indices 1) childIdx tree 
                        Err _ -> Err  "no such value"                              
                Err _ -> Err  "no such value"
    
        Err _ ->  Err  "no such value"
     


modifyActive = \ tree, headId, op ->
    when Dict.get tree.content headId  is 
        Ok head -> 
            if List.isEmpty head.children == Bool.true then
                op headId tree
            else 
                when List.last head.children is 
                    Ok nodeId ->
                        when Dict.get tree.content nodeId is 
                            Ok child ->
                                if child.locked == Bool.true then
                                    op headId tree
                                else 
                                    modifyActive tree nodeId op
                            Err _ -> Err  "internal logic error"
                    Err _ -> Err  "internal logic error"
                        
        Err _ -> Err  "wrong tree node id"


changeValue = \ tree, id, value-> 
    modify = 
        \ idValue, treeValue ->
            when Dict.get treeValue.content idValue is 
                Ok node ->
                    Ok (changeElement treeValue idValue {node & value : List.append node.value value  } )
                Err _ -> Err  "internal logic error"

    when modifyActive tree id modify is 
        Ok newTree -> newTree
        _ -> tree
         

seekInTreeOnlyExcept = ( \ head, tree, errMess -> 
            List.walk head.children (Ok { characterSet : [], limitRanges : [] }) ( \ state, childId ->
                when state is 
                    Ok items ->
                            
                        when getDirectlyChildrenCnt tree childId is 
                            Ok  cnt ->
                                
                                    if cnt == 1 then 
                                        when  (getValue [0] childId tree) is 
                                            Ok val -> 
                                                Ok { items & characterSet :  List.concat items.characterSet  val  }
                                            Err _ -> Err (Str.concat errMess "wrong second level child structure, missing value" )

                                    else if cnt == 2 then
                                        getFirst = (\  valLst ->
                                            when List.first valLst is 
                                                Ok val -> Ok val
                                                Err _ -> Err  "no value"
                                            )
                                                                      
                                        when (getValue [0] childId tree) is    
                                            Ok val1Lst -> 
                                                when getFirst val1Lst is
                                                    Ok val1 ->
                                                        when (getValue [1] childId tree)  is
                                                            Ok val2Lst ->
                                                                when getFirst val2Lst is
                                                                    Ok val2 -> 
                                                                        Ok { items & limitRanges :  List.append items.limitRanges  { left : val1, right : val2 } }
                                                                        
                                                                    Err _ -> Err (Str.concat errMess "wrong second level child structure, missing value 2" )
                                                            Err _ -> Err (Str.concat errMess "wrong second level child structure, missing value 2" )
                                                    Err _ -> Err (Str.concat errMess "wrong second level child structure, missing value 1" )
                                                    
                                            Err _ -> Err (Str.concat errMess "wrong second level child structure, missing value 1" )

                                    else 
                                        Err (Str.concat errMess "wrong second level child structure" )
                            Err _ -> Err (Str.concat errMess "wrong second level child structure" )
                                
                    Err  message -> Err  message  
                ))

createExceptOutOfTree =  \ tree ->
    errMess = "Except token problem:  "
    when Dict.get tree.content 0  is 
        Ok head ->
            searchResult = seekInTreeOnlyExcept head tree errMess
            when  searchResult is 
                Ok search  ->
                    if List.isEmpty search.characterSet == Bool.false &&
                        List.isEmpty search.limitRanges == Bool.false then
                        chain =
                            []
                            |> List.append (CharacterSet search.characterSet)
                            |> List.append (LimitRanges search.limitRanges)
                            
                        Ok (createToken (Except  chain) Once Bool.false )
        
                    else if List.isEmpty search.characterSet == Bool.false then 
                        Ok (createToken (Except [CharacterSet search.characterSet]) Once Bool.false ) 
                    else if List.isEmpty search.limitRanges == Bool.false then
                        Ok (createToken (Except [LimitRanges search.limitRanges]) Once Bool.false )
                    else 
                        Err (Str.concat errMess "empty tree") 
                    
                Err message -> Err message
        Err _ -> Err  (Str.concat errMess "not right format of tree")


createOnlyOutOfTree =  \ tree ->
    errMess = "Only token problem:  "
    when Dict.get tree.content 0  is 
        Ok head ->
            searchResult = seekInTreeOnlyExcept head tree errMess
            when  searchResult is 
                Ok search  ->
                    if List.isEmpty search.characterSet == Bool.false &&
                        List.isEmpty search.limitRanges == Bool.false then
                        chain =
                            []
                            |> List.append (createToken CaptureOpen Once Bool.false )
                            |> List.append (createToken (CharacterSet search.characterSet) Once Bool.false )
                            |> List.append (createToken CaptureClose Once Bool.false )
                            |> List.append (createToken Separator Once Bool.false )
                            |> List.append (createToken CaptureOpen Once Bool.false )
                            |> List.append (createToken (LimitRanges search.limitRanges) Once Bool.false )
                            |> List.append (createToken CaptureClose Once Bool.false )
                            
                        Ok (createToken (Sequence  chain) Once Bool.false )
        
                    else if List.isEmpty search.characterSet == Bool.false then 
                        Ok (createToken (CharacterSet search.characterSet) Once Bool.false ) 
                    else if List.isEmpty search.limitRanges == Bool.false then
                        Ok (createToken (LimitRanges search.limitRanges) Once Bool.false )
                    else 
                        Err (Str.concat errMess "empty tree") 
                    
                Err message -> Err message
        Err _ -> Err  (Str.concat errMess "not right format of tree")
                    
                    
                    
createRepetitionOutOfTree =  \ tree ->
    errMess = "Repetition token problem:  "
    when Dict.get tree.content 0  is 
        Ok head ->
            searchResult = List.walkUntil head.children (Ok []) ( \ stateResult, childId ->
                when  stateResult  is 
                    Ok state ->
                        when  (getValue [0] childId tree) is 
                            Ok valLst ->
                                when valLst is 
                                    [ val ] ->  
                                        Continue (Ok  (List.append state  val) )
                                    _ -> Break ( Err (Str.concat errMess "wrong child structure" )) 
                            Err _ -> 
                                Break (Err (Str.concat errMess "wrong child structure, missing value" ))
                    Err message -> Break (Err message)
                )
                
            when searchResult is 
                Ok search ->
                    when search is  
                        [val] ->
                            Ok ( NTimes val )
                                        
                        [val1, val2] ->
                            if  val1 < val2  then 
                                Ok (MNTimes val1 (val2 - val1) )
                            else 
                                Err  ( Str.concat errMess " first value greather than second " )
                        _  ->  Err  ( Str.concat errMess "wrong structure" )
                
                Err message  -> Err message  
        Err  _  ->  Err  ( Str.concat errMess " no values " )

                    
composeMatchedStructure = \ tree, id,tag ->
    
    addNewNode = \ idNew, treeNew -> Ok (addElement treeNew idNew emptyNode)
    
    lockNode = 
        \ idLock, treeLocked ->
            when Dict.get treeLocked.content idLock is 
                Ok node ->
                    Ok (changeElement treeLocked idLock {node & locked : Bool.true } )
                Err _ -> Err  "internal logic error"
    result = 
        when tag is 
            CaptureOpen -> 
                if Dict.isEmpty tree.content == Bool.false then 
                    modifyActive tree id addNewNode 
                else
                    Err  "internal logic error"
            CaptureClose ->
                modifyActive tree id lockNode
    when result is
        Ok newTree -> newTree
        Err _  -> tree
         
    
# tree used is  awkward consider  using  something along those  lines  in the future
#modifyActive = \ op, currentStructHead  ->
#    Node  head = currentStructHead
#    dbg  head
#    updatedLst = 
#        if List.isEmpty head.children == Bool.true then
#            op head.children
#        else 
#            when List.last head.children is 
#                Ok node ->
#                    Node child = node 
#                        if child.locked == Bool.true then
#                            op head.children
#                        else 
#                            modifyLastInList head.children  (modifyActive op  node )                                   
#                Err _ -> []           
#    Node {head & children : updatedLst }
 
 

#composeMatchedStructure = \ tag, currentStructHead -> 
#    when tag is 
#        CaptureOpen -> 
#            modifyActive (\ lst -> List.append lst { locked : Bool.false, children : Children [], value : [] } ) currentStructHead
#        CaptureClose ->
#            modifyActive
#                 (\ lst ->
#                    when List.last lst is 
#                        Ok elem ->
#                            modifyLastInList lst { elem & locked : Bool.true} 
#                        Err _ -> [] )
#                 currentStructHead 

availableRegex = stagesCreationRegex [] 

parseStr = \ str, pattern -> 
    
    when availableRegex is 
        Ok stage1Pat -> 
            tokensFromUserInputResult = regexCreationStage2 pattern stage1Pat  []
            
            when tokensFromUserInputResult is 
                Ok tokensFromUserInput ->
                    independentChainlst = splitChainOnSeparators tokensFromUserInput []
                    
                    # for now get longest maybe??
                    
                    Ok (List.walk independentChainlst (createParsingRecord [] Inactive)  ( \ state, regexParser ->  
                        parsResult = checkMatching (Str.toUtf8  str ) regexParser    
                        if state.result == Bool.true then
                            if List.len parsResult.matched > List.len state.matched  then 
                                parsResult
                            else 
                                state
                            else 
                                parsResult ))
                Err message -> 
                    Err (Str.concat "You screwed up something, or not supported construction, or internal bug \n"  message )

        Err message -> 
            Err  (Str.concat "This is internal regex error not your fault\n"  message )

# availableRegex = stagesCreationRegex [] 
    
main =
    pattern = "aaaa"
    # pp =  parseStr  "sss"   "a"

    avil  =availableRegex

    testModify = ( \ tree,fun, value  -> 
        fun tree  0 value )
            
    res = 
        testModify  treeBase  composeMatchedStructure   CaptureOpen  
        |> testModify  composeMatchedStructure   CaptureOpen
        |> testModify  changeValue  1
        |> testModify  composeMatchedStructure   CaptureClose
        |> testModify  composeMatchedStructure   CaptureOpen
        |> testModify  changeValue  10
        |> testModify  composeMatchedStructure   CaptureClose
        |> testModify  composeMatchedStructure   CaptureClose
        |> testModify  composeMatchedStructure   CaptureOpen
        |> testModify  composeMatchedStructure   CaptureOpen
        |> testModify  changeValue  3
        |> testModify  changeValue  100
        |> testModify  composeMatchedStructure   CaptureClose
        |> testModify  composeMatchedStructure   CaptureClose

    chainOut = 
        []
        |> List.append  (createToken  CaptureOpen  Once Bool.false )
        |> List.append  (createToken (Sequence  [createToken  Dot  AtLeastOne Bool.true])  Once Bool.false)
        |> List.append  (createToken  CaptureClose  Once Bool.false)
            
    pat  =
        []
        |> List.append  (createToken  ( Character '[' )  Once Bool.false )
        |> List.append  (createToken  CaptureOpen  Once Bool.false)
        |> List.append  (createToken (Sequence  chainOut)  Once Bool.false) 
        |> List.append  (createToken  CaptureClose  Once Bool.false)
        |> List.append  (createToken  ( Character ']' )  Once Bool.false)
        
    
                
    Stdout.line "outStr"
    
    

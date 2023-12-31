interface Regex
    exposes [parseStr,getValue ]
    imports []
    #packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    #imports [pf.Stdout]
    #provides [main] to pf

# known  issues 
# Regex.parseStr "[1;16R" "(\\d+);(\\d+)"  - seems like this matches 1 and 1 istead of  1 16 



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
    { tag : Except, str : "[^(((.)-(.))|((.))|((\\T.?{}^*T)))+]" }, # "
    { tag : Only, str : "[(((.)-(.))|((.))|((\\T.?{}^*T)))+]" } # "
    ]  # "
        

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
    |> Dict.insert Except 3
    
charToUtf : Str -> U8
charToUtf = \ char ->
    Str.toUtf8 char
    |> List.first
    |> Result.withDefault 0

emptyNode = { locked : Bool.false, children :  [], value : [] }

treeBase = 
    {cnt : 0, content : Dict.empty {} }
    |> addElement  0 emptyNode
    
createParsingRecord = \ regex, meta, strict ->
    { regex : regex, current : regex, matched : [], result : Bool.false, missed : [], left : [] , captured : treeBase, meta : meta, strict : strict }


checkMatchingValidity = \ matchingResult ->
    when matchingResult.strict is 
        No -> Bool.true 
        Front ->
            List.isEmpty  matchingResult.missed
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

# if  I try to  enable  types  there is  crash :(
#createToken :  TagType  (U8) (Int b), SerieType (Int b), Bool -> TokenType (U8) (Int b) where a implements Hash & Eq, b implements Hash & Eq
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
    #matchStr : U8, TagType  ( Num a ) (Num  b)-> [NoMatch, Consume ] where a implements Hash & Eq, b implements Hash & Eq
    matchStrBugPrevent = \ utf, pattern ->
        checkRangeBug = ( \ val, ranges -> 
                        List.walkUntil ranges Outside  (\ _state, elem ->
                            if val >= elem.left  && val <= elem.right then
                                Break Within
                            else
                                Continue Outside ))
        
        when pattern is 
            LimitRanges lst -> 
                when checkRangeBug utf lst is 
                    Within -> Consume
                    Outside -> NoMatch
            CharacterSet lst ->
                when List.findFirst lst  ( \ char -> char == utf ) is 
                    Ok _ -> Consume
                    Err _ -> NoMatch 
            _ -> NoMatch

    matchStr = \ utf, pattern ->

        checkRange = ( \ val, ranges -> 
                        List.walkUntil ranges Outside  (\ _state, elem ->
                            if val >= elem.left  && val <= elem.right then
                                Break Within
                            else
                                Continue Outside ))
        
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
                List.walkUntil tokens Consume ( \  state , token  ->
                #  because below  crashes
                #    when matchStr  utf  token is
                    when matchStrBugPrevent  utf  token is
                        Consume -> Break  NoMatch
                        NoMatch -> Continue state 
                    ) 
                    
                
            Character val ->
                if val == utf then
                    Consume    
                else
                    NoMatch
            Dot ->
                Consume
            _ -> NoMatch
    
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
                                chains = splitChainOnSeparators chain []
                                when pat.serie is 
                                    AtLeastOne ->   
                                        changeFront = 
                                            (List.dropFirst regItem.current 1)
                                            |> List.prepend { pat & serie : ZeroOrMore }
                                        
                                        List.walk  chains  state  (  \ inState, inChain  ->
                                            List.concat inChain changeFront
                                            |> (\ updatedCurrent ->  
                                                    List.concat inState (updateRegexItemInternal [] {regItem & current : updatedCurrent} ))
                                            |> ( \ updatedState ->
                                                List.concat inChain (List.dropFirst regItem.current 1)
                                                |> (\ updatedCurrent ->  
                                                        List.concat updatedState (updateRegexItemInternal [] {regItem & current : updatedCurrent}) ))
                                        )
                                    ZeroOrMore ->  
                                        List.walk  chains  state  (  \ inState, inChain  ->
                                            List.concat inState
                                                (List.concat inChain  regItem.current
                                                |> (\ updatedCurrent -> ( updateRegexItemInternal [] {regItem & current : updatedCurrent} )))
                                        )
                                        |> List.concat  ( updateRegexItemInternal [] {regItem & current : (List.dropFirst regItem.current 1)} )
                                    
                                    MNTimes m n -> 
                                        changeFront = 
                                            (List.dropFirst regItem.current 1)
                                            |> List.prepend { pat & serie : NoMorethan n }
                                            
                                        List.walk  chains  state  (  \ inState, inChain  ->   
                                            List.concat (concatIter m inChain  [] ) changeFront
                                            |> (\ updatedCurrent -> 
                                                    List.concat inState ( updateRegexItemInternal [] {regItem & current : updatedCurrent} ) )
                                        )
                                    NoMorethan cnt -> 
                                        if  cnt == 0 then
                                            List.concat state  ( updateRegexItemInternal [] {regItem & current : (List.dropFirst regItem.current 1)} )
                                        else 
                                            changeFront = 
                                                (List.dropFirst regItem.current 1)
                                                |> List.prepend { pat & serie : NoMorethan (cnt - 1) }
                                            List.walk  chains  state  (  \ inState, inChain  -> 
                                                List.concat inChain changeFront
                                                |> (\ updatedCurrent ->  
                                                    List.concat inState (updateRegexItemInternal [] {regItem & current : updatedCurrent} ))
                                            )   
                                            |> List.concat  ( updateRegexItemInternal [] {regItem & current : (List.dropFirst regItem.current 1)} )
                                
                                    NTimes cnt -> 
                                        List.walk  chains  state  (  \ inState, inChain  ->
                                            List.concat (concatIter cnt inChain  [] ) (List.dropFirst regItem.current 1)
                                            |> (\ updatedCurrent -> 
                                                    List.concat inState ( updateRegexItemInternal [] {regItem & current : updatedCurrent} ) )  
                                        )
                                    Once ->
                                        List.walk  chains  state  (  \ inState, inChain  ->
                                            List.concat inChain (List.dropFirst regItem.current 1)
                                            |> (\ updatedCurrent ->  
                                                List.concat inState (updateRegexItemInternal [] {regItem & current : updatedCurrent}) )  
                                        )
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
                                            |> (\ updatedCurrent ->  {regItem & current : updatedCurrent} ))
                                    
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
                                            |> List.concat  ( updateRegexItemInternal [] {regItem & current : (List.dropFirst regItem.current 1)} )
                                
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
                    Inactive {state & meta : Inactive } 
            )

    regeneratePat = (\  state  -> 
        when List.first state.current is
            Ok pat -> 
                Old state
            Err _ ->  
                # if state.meta == Origin then
                    # BUG  in this  line
                    # getFirstPat  { state & current : state.regex } 
                #    New { regex : state.regex, current : state.regex, matched : state.matched, result : state.result, missed : state.missed, left : state.left, captured : state.captured, meta : state.meta, strict : state.strict } 
                # else 
                New {state & meta : Inactive }  )
            
            
    checkLastListEmpty = (\ listOfLists  ->
            when List.last listOfLists is
                Ok lst -> 
                    List.isEmpty lst
                Err _ ->
                    Bool.false  
    
        )
    # workaround: I haven't predicted that this information will be needed.
    # on earlier stages it would be cleaner (maybe)to extract this but not with this design
    # I just missed that opportunity and now I don't want to rip through entire thing to make it right
    regexStartEndSetting = 
        when (List.first reg, List.last reg ) is
            (Ok  (  tokenFirst) , Ok (  tokenLast) ) ->
                when ( tokenFirst.token, tokenLast.token )  is 
                    (Character  first,Character  last )  ->
                        if first == '^' && last == '$' then
                            { updatedRegex : 
                                List.dropFirst reg 1
                                |> List.dropLast 1,
                                strict : Both }
                        else if first == '^'  then
                            { updatedRegex : List.dropFirst reg 1, strict : Front }
                        else if last == '$'  then
                            { updatedRegex : List.dropLast reg 1, strict : Back } 
                        else
                            { updatedRegex : reg, strict : No }          
                    (Character  first, _) ->
                        if first == '^'  then 
                            { updatedRegex : List.dropFirst reg 1, strict : Front }
                        else
                            { updatedRegex : reg, strict : No }    
                        
                    (_, Character  last) ->
                        if last == '$'  then
                            { updatedRegex : List.dropLast reg 1, strict : Back }
                        else
                            { updatedRegex : reg, strict : No }    
                        
                    _ -> 
                        { updatedRegex : reg, strict : No }
            _ ->  { updatedRegex : reg, strict : No }
    

    complexSearch = 
        List.walk utfLst { missed : [] , parsing : [createParsingRecord regexStartEndSetting.updatedRegex Active  regexStartEndSetting.strict]}  ( \ outState, utf ->
            
            updatedStates = updateRegex outState.parsing 
            List.walk updatedStates [] ( \ state, processedReg ->   
                
                # it is  stupid  but without  this line it is crashing
                a  =  printMe  processedReg.regex
                
                
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
                                            
                                            allowEnd = \ lst, allowInState ->
                                                List.walkUntil lst {allow: Bool.true, state : allowInState }  ( \allowState, elem ->
                                                    if elem.token == CaptureClose then 
                                                        allowStateUpdate = allowState.state
                                                        Continue {allowState & state : { allowStateUpdate &   captured : composeMatchedStructure allowStateUpdate.captured  0 CaptureClose } }
                                                        
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
                                                curState
                                             
                                        _ -> curState  
                                )
                    manageIteration processedReg state )
                    |> ( \ parsingSet  -> 
                        updatedMissed = List.append outState.missed utf
                        parsingState = createParsingRecord regexStartEndSetting.updatedRegex Active  regexStartEndSetting.strict
                        { missed : updatedMissed,
                        parsing : List.append  parsingSet { parsingState & missed : updatedMissed}  } )
                     
        )
    List.walk complexSearch.parsing  (createParsingRecord reg Inactive  No) ( \ state, parsResult -> 

                
        updatedParsResult  =  { parsResult &  result :(checkMatchingValidity parsResult) && parsResult.result  }  
        
        if state.result == Bool.true then
            if List.len updatedParsResult.matched > List.len state.matched  then 

                updatedParsResult
            else 
                state
        else 
            updatedParsResult )
    
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
                List.walk results (Ok { lst : [] , capture : 0 }) ( \ outState, result  ->
                    when outState is 
                        Err message -> Err message 
                        Ok state ->
                            doCapture = \ capture  ->
                                capture > 0
                            
                            
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
                                                        
                                                
                                            Err _ -> Err "Wrong usage of +/*/Repetition/? in pattern"        
                                        )
                 
                            when  result.tag is 
                                Character ->
                                    when List.first result.parsedResult.matched is 
                                        Ok  matched  ->
                                            Ok { state &  lst : modifLastInChain state.lst  (createToken (Character matched) Once (doCapture state.capture)) }
                                        Err _ -> Err "parser  match problem"
                                    
                                Dot ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken Dot Once (doCapture state.capture) )  }
                                CaptureOpen ->
                                    
                                    openLst = 
                                        modifLastInChain state.lst (createToken CaptureOpen Once Bool.false)
                                        |> modifLastInChain  (createToken (Sequence []) Once Bool.false)
                                    Ok { lst : openLst, capture : state.capture + 1}
                                CaptureClose ->
                                    Ok { lst : modifLastInChain state.lst (createToken CaptureClose Once Bool.false), capture : state.capture - 1 }
                                Separator -> 
                                    Ok { state &  lst : modifLastInChain state.lst (createToken Separator Once Bool.false )  }
                                
                                Digit ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken Digit Once (doCapture state.capture) )  }
                                NoDigit ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken NoDigit Once (doCapture state.capture) )  }
                                Alphanumeric ->
                                    limitRangToken = LimitRanges [{ left : 'A', right : 'Z' },{ left : 'a', right : 'z' },{ left : '0', right : '9' },{ left : '_', right : '_' }]
                                    Ok { state &  lst : modifLastInChain state.lst (createToken limitRangToken Once (doCapture state.capture) )  }
                                NoAlphanumeric ->
                                    limitRangToken = ReverseLimitRanges [{ left : 'A', right : 'Z' },{ left : 'a', right : 'z' },{ left : '0', right : '9' },{ left : '_', right : '_' }]
                                    Ok { state &  lst : modifLastInChain state.lst (createToken limitRangToken Once (doCapture state.capture) )  }
                                Whitespace ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken (CharacterSet [0x20, 0x09, 0x0D, 0x0A]) Once (doCapture state.capture) ) }
                                NoWhitespace ->
                                    Ok { state &  lst : modifLastInChain state.lst (createToken (NotInCharacterSet [0x20, 0x09, 0x0D, 0x0A]) Once (doCapture state.capture) ) }
                                Only -> 
                                    when createOnlyOutOfTree result.parsedResult.captured is
                                        Ok onlyToken -> 
                                            Ok { state &  lst : modifLastInChain state.lst onlyToken }
                                        Err message ->
                                            Err message
                                Except ->
                                    when createExceptOutOfTree result.parsedResult.captured is
                                        Ok exceptionToken ->
                                            Ok { state &  lst : modifLastInChain state.lst exceptionToken }
                                        Err message ->
                                            Err message
                                BackSlash->
                                    when result.parsedResult.matched is 
                                        [backslash, sign ] ->
                                            Ok { state &  lst : modifLastInChain state.lst  (createToken (Character sign) Once (doCapture state.capture)) }
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
                        Err _ -> Err  "children missing"                              
                Err _ -> Err  "parent node missing"
    
        Err _ ->  Err  "provide ate least one indice"
     


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
    List.walk head.children (Ok { characterSet : [], limitRanges : [] }) ( \ stateOutResult, topChildIdId ->
        when Dict.get tree.content topChildIdId is
            Ok childHead ->
                childResult =
                    List.walk childHead.children (Ok { characterSet : [], limitRanges : [] }) ( \ state, childId ->
                        when state is 
                            Ok items ->
                                    
                                when getDirectlyChildrenCnt tree childId is 
                                    Ok  cnt ->
                                            if cnt == 1 then 
                                                when  (getValue [0] childId tree) is 
                                                    Ok val -> 
                                                        when val is 
                                                            [ '\\', secVal ] ->
                                                                Ok { items & characterSet :  List.append items.characterSet  secVal  }
                                                            _ -> 
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
                        )
                when stateOutResult is 
                    Ok stateOut ->
                        when childResult is 
                            Ok result ->
                                Ok { characterSet : List.concat  result.characterSet stateOut.characterSet,
                                limitRanges : List.concat result.limitRanges stateOut.limitRanges}        
                            Err  message -> Err  message    
                    Err  message -> Err  message         
                
            Err  _ -> Err (Str.concat errMess "wrong structure" )  
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

                        when  (getDirectly tree childId ) is 
                            Ok valLst ->
                                when Str.fromUtf8 valLst is
                                    Ok strVal ->
                                        when Str.toU32 strVal  is 
                                            Ok numVal ->
                                                Continue (Ok  (List.append state  numVal) )
                                            Err _ -> Break (Err (Str.concat errMess "wrong child structure, incompatible value" ))     
                                    Err _ ->
                                        Break (Err (Str.concat errMess "wrong child structure, incompatible value" ))                 
            
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
                                Ok (MNTimes val1  (val2 - val1) )
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
                    Ok (List.walk independentChainlst (createParsingRecord [] Inactive No)  ( \ state, regexParser ->  
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



SerieType a: [ AtLeastOne, ZeroOrMore, MNTimes a  a, NoMorethan a, NTimes a, Once ] where a implements Hash & Eq

TokenType a b: { token :TagType a , serie : SerieType b, capture : Bool  } where a implements Hash & Eq, b implements Hash & Eq

TagType a b : [ Dot, CaptureClose, CaptureOpen, ReverseLimitRanges ( List { left : b, right : b } ),
            Separator,  CharacterSet (List a), NotInCharacterSet (List a), 
            Except (List (TagType a b)), Character  a , Digit, NoDigit, LimitRanges ( List { left : b, right : b } ),
            Sequence (List  { token :TagType a b, serie : SerieType b, capture : Bool  }) ]  where a implements Hash & Eq, b implements Hash & Eq

#ArrayType : {values: List a ,branch : [Children arrayType] }
#NodeType a  b :  { children : List (Num a), locked : Bool, value : List b} where a implements Hash & Eq
#TreeType a: { cnt : Num a, content : Dict (Num a) { children : List (Num a)}  } where a implements Hash & Eq
#createTreePattern : treeType, arrayType, (Num a) -> nodeType where a implements Hash & Eq
#createTreePattern = \ tree, lst, parentId ->
    #node  = { locked : Bool.true, children :  [], value : lst.values }
    #updatedTree = addElement  tree parentId node 
    #Children children = lst.branch
    #createTreePattern updatedTree children tree.cnt
            




#################### succeed/failed tests ####################
    
# rudimentary tests
expect
    when parseStr "a" "a" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test characters matching"
               
expect
    when parseStr "b" "a" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test characters matching"

expect
    when parseStr "aaaabcc" "abc" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test characters matching"  
        
expect
    when parseStr "aaaabbcabcadsa" "abc" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test characters matching"  
        
expect
    when parseStr "aaaabcdfa" "abcdef" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test characters matching"
        
expect
    when parseStr "aghdsad" "." is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test dot matching"
        
expect
    when parseStr "agh" "...." is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test dot matching"
        
expect
    when parseStr "abcd323sfsddgf" "\\d\\d\\d" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test digit matching"
        
expect
    when parseStr "abcd32sfsddgf" "\\d\\d\\d" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test digit matching"
        
expect
    when parseStr "24423abd45236436" "\\D\\D\\D" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test digit matching"
        
expect
    when parseStr "24423ab3d45236436" "\\D\\D\\D" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test digit matching"
        
expect
    when parseStr "24423...45236436" "\\.\\.\\." is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test period matching"
        
expect
    when parseStr "24423..3d.45236436" "\\.\\.\\." is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test period matching"
        
expect
    when parseStr "2A_d" "\\w\\w\\w\\w" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test alphanumeric matching"
        
expect
    when parseStr "2A_]{d" "\\w\\w\\w\\w" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test alphanumeric matching"
            
expect
    when parseStr "#@$a%^&*" "\\W\\W\\W\\W" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test non-alphanumeric matching"
        
expect
    when parseStr "#@$a%^_&*" "\\W\\W\\W\\W" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test non-alphanumeric matching"    
        
expect
    str = "  dd  \n\rdsda   d "
    when parseStr str "\\s\\s\\s\\s" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test whitespaces matching"
        
expect
    str = "  dd  \ndsda   d "
    when parseStr str "\\s\\s\\s\\s" is 
    Ok parsed ->
        parsed.result == Bool.false
    Err mes -> mes == "test whitespaces matching"
        
expect
    str = "  dd  \n\rdsda   d "
    when parseStr str "\\S\\S\\S\\S" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test non-whitespaces matching"
        
expect
    str = "  dd  \ndsd\ra   d "
    when parseStr str "\\S\\S\\S\\S" is 
    Ok parsed ->
        parsed.result == Bool.false
    Err mes -> mes == "test non-whitespaces matching"  

expect
    when parseStr "abcd" "ab?c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test optional character matching"

expect
    when parseStr "acd" "ab?c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test optional character matching"

expect        
    when parseStr "abbcd" "ab?c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test optional character matching"

expect
    when parseStr "abcd" "a(bc)?d" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test optional characters matching"
 
 expect
    when parseStr "ad" "a(bc)?d" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test optional characters matching"

expect 
    when parseStr "abd" "a(bc)?d" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test optional characters matching"
      
expect  
    when parseStr "abcbcd" "a(bc)?d" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test optional characters matching"
        
expect        
    when parseStr "abbbcd" "ab*c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test zero or more matching"

expect        
    when parseStr "acd" "ab*c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test zero or more matching"

expect
    when parseStr "abcbcbcdfbcdbgc" "a(bc)*d" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test zero or more matching"  
        
expect
    when parseStr "abcfdadbgc" "a(bc)*d" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test zero or more matching"  
        
expect
    when parseStr "abxcfbxcdbgc" "a(bc)*d" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test zero or more matching" 
        
expect        
    when parseStr "abbbcd" "ab+c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test at least one matching"

expect        
    when parseStr "acd" "ab+c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test at least one matching"

expect
    when parseStr "abcbcbcdfbcdbgc" "a(bc)+d" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test at least one matching"  
        
expect
    when parseStr "abcfdadbgc" "a(bc)+d" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test at least one matching"  
        
expect
    when parseStr "abxcfbxcdbgc" "a(bc)+d" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test at least one matching"  
        
expect        
    when parseStr "abbbcd" "ab{3}c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test repetition matching" 
        
expect        
    when parseStr "abbbbbbbbbbbcd" "ab{11}c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test repetition matching" 
        
expect        
    when parseStr "abbbbcd" "ab{3}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition matching" 

expect        
    when parseStr "abbcd" "ab{3}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition matching" 
        
expect        
    when parseStr "abbbbbbcd" "a(bb){3}c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test repetition matching" 
        
expect        
    when parseStr "abbbbbbbcd" "a(bb){3}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition matching" 
        
expect        
    when parseStr "abbcd" "a(bb){3}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition matching"
        
expect        
    when parseStr "abcd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when parseStr "abbcd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when parseStr "acd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition range matching"  
        
        
expect        
    when parseStr "abbbcd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when parseStr "abbbbbbbbbbcd" "ab{10,11}c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test repetition range matching" 
        
expect        
    when parseStr "abbbbbbbbbcd" "ab{10,11}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition range matching"
        
expect        
    when parseStr "abcd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when parseStr "abbcd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when parseStr "abbbbcd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when parseStr "abbbcd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when parseStr "abbbbbbcd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition range matching"  
         
expect        
    when parseStr "acd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test repetition range matching"  
         
expect        
    when parseStr "dad" "d[a-g]d" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test one of matching"  
         
expect        
    when parseStr "abhd" "ab[a-g]d" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test one of matching"  
expect        
    when parseStr "a6a" "a[a-g1-7]a" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test one of matching" 
        
expect        
    when parseStr "a8a" "a[a-g1-7]a" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test one of matching" 
        
expect        
    when parseStr "zcz" "z[abcd]z" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test one of matching" 
        
expect        
    when parseStr "zfz" "z[abcd]z" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test one of matching" 
        
expect        
    when parseStr "z*z" "z[\\*\\?\\{]z" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test one of matching" 
        
expect        
    when parseStr "z^z" "z[\\*\\?\\{]z" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test one of matching" 
        
expect        
    when parseStr "dad" "d[^a-g]d" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test except of matching"      
        
expect        
    when parseStr "abhd" "ab[^a-g]d" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test except of matching"  
expect        
    when parseStr "a6a" "a[^a-g1-7]a" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test except of matching" 
        
expect        
    when parseStr "a8a" "a[^a-g1-7]a" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test except of matching" 
        
expect        
    when parseStr "zcz" "z[^abcd]z" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test except of matching" 
        
expect        
    when parseStr "zfz" "z[^abcd]z" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test except of matching" 
        
expect        
    when parseStr "z*z" "z[^\\*\\?\\{]z" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test except of matching" 
        
expect        
    when parseStr "z^z" "z[^\\*\\?\\{]z" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test one of matching" 
        
expect        
    when parseStr "abcdefgh" "^abc" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test start end matching"     
        
expect        
    when parseStr "ffggabcdefgh" "^abc" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test start end matching"  
        
expect        
    when parseStr "aabc" "abc$" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test start end matching"     
        
        
expect        
    when parseStr "ffggabcde" "abc$" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test start end matching"   
        

expect        
    when parseStr "abc" "^abc$" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test start end matching"  
        
expect        
    when parseStr "aabc" "^abc$" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test start end matching"     
        
        
expect        
    when parseStr "abcde" "^abc$" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test start end matching"
        
expect        
    when parseStr "ffggabcdeabc" "^abc|abc$" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test start end matching"  
          
expect        
    when parseStr "ffggaaxyzbc" "abc|axyz|jjjj" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test separator matching"  
          
expect        
    when parseStr "ffggjjjjzbc" "abc|axyz|jjjj" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test separator matching"  
          
expect        
    when parseStr "ffggjjjzbc" "abc|axyz|jjjj" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test separator matching"  
          
expect        
    when parseStr "aaaccaaaa" "aa(bb|cc)aa" is 
        Ok parsed ->
            parsed.result == Bool.true
        Err mes -> mes == "test separator matching"  
          
expect        
    when parseStr "aaacccaaaa" "aa(bb|cc)aa" is 
        Ok parsed ->
            parsed.result == Bool.false
        Err mes -> mes == "test separator matching"  
          
# more  complex  test

#for now I consider research phase as done , I should move to stabilisation phase now
#but I will postpone this a bit. 
#so in near future, I need to do type annotations and conduct fair bit of more comprehensive testing   

    
    

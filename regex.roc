    app "reg"

    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

# simplified, one level capture, nothing more needed  in context of peek (at least for now ), it is just easy this way 
# in  general more I dig into it, more and more corner cases come to a surface. I think I don't need to deal with all of them. I will limit myself to use cases vailid in context of peek app 
# dbg crashing left and right on some of more complex object
priorities =
    Dict.empty {}
    |> Dict.insert Empty -1
    |> Dict.insert Character 0
    |> Dict.insert Dot 1
    |> Dict.insert CaptureOpen 2
    |> Dict.insert CaptureClose 3
    |> Dict.insert AtLeastOne 4

charToUtf = \ char ->
    Str.toUtf8 char
    |> List.first
    |> ( \ res ->
        when res is 
            Ok utf8 -> utf8
            Err _ -> 0  ) 
        

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
    
decorate = \ tokens ->
    tokens

createToken = \ token, serie, capture ->
    { token :token, serie : serie, capture : capture  }

regexSeedPattern = [
    { tag : Character, tokens : [ createToken  Dot  Once Bool.false ] } ]



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
            Range lst ->
                when List.findFirst lst  ( \ char -> char == utf ) is 
                    Ok _ -> Consume
                    Err _ -> NoMatch 
            NotInRange lst ->
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
        List.walk  regex [] ( \ state, regItem ->
            when List.first regItem.current is 
                Ok pat ->
                    when pat.token  is
                        Sequence  chain ->
                            when pat.serie is 
                                AtLeastOne ->           
                                    changeFront = 
                                        (List.dropFirst regItem.current)
                                        |> List.prepend { pat & serie : ZeroOrMore }
                                        
                                    List.concat chain changeFront
                                    |> (\ updatedCurrent ->  List.append state {regItem & current : updatedCurrent} )  
                                ZeroOrMore ->

                                    List.append state {regItem & current : (List.dropFirst regItem.current)}  
                                    |> List.append 
                                        (List.concat chain (List.dropFirst regItem.current)
                                        |> (\ updatedCurrent ->  {regItem & current : updatedCurrent, meta : Active} ))
                                NTimes cnt -> 
                                    concatIter = (\ n , lst, stored ->
                                        if n == 0 then 
                                            stored
                                        else 
                                            concatIter (n-1) lst (List.concat lst stored  ))
                                    
                                    List.concat (concatIter cnt chain  [] ) (List.dropFirst regItem.current)
                                    |> (\ updatedCurrent ->  List.append state {regItem & current : updatedCurrent} )  
                                Once ->
                                    List.concat chain (List.dropFirst regItem.current)
                                    |> (\ updatedCurrent ->  List.append state {regItem & current : updatedCurrent} )  
                        _ -> List.append state regItem
                    
                Err _ -> List.append state regItem )
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
                        getFirstPat  { regex : state.regex, current : state.regex, matched : state.matched, result : state.result, missed : state.missed , left : state.left, captured : state.captured, meta : state.meta } 
                    else   
                        Inactive {state & meta : Inactive } )



    checkLastListEmpty = (\ listOfLists  ->
            when List.last listOfLists is
                Ok lst -> 
                    List.isEmpty lst
                Err _ ->
                    Bool.false  
    
        )

    complexSearch = 
        List.walk utfLst [{ regex : reg, current : reg, matched : [], result : Bool.false, missed : [], left : [] , captured : [], meta : Origin}]  ( \ outState, utf ->
            updatedStates = updateRegex outState 
            
            List.walk updatedStates [] ( \ state, processedReg ->   

                if processedReg.result == Bool.true then
                    List.append state { regex : processedReg.regex, current : processedReg.current, matched : processedReg.matched, result : processedReg.result, missed : processedReg.missed , left : List.append  processedReg.left utf, captured : processedReg.captured, meta  : processedReg.meta} 
                else
                    
                     
                    manageIteration = ( \ inProcessedReg,curState -> 
                        when getFirstPat inProcessedReg is
                            Inactive patternSet ->
                                List.append curState inProcessedReg
                            Active matchThis ->
                                toUpdateState = matchThis.state  
                                if matchThis.pattern.token == CaptureOpen then
                                    tmpState = {toUpdateState & current : List.dropFirst toUpdateState.current }
                                    captureUpdateOpen = 
                                        if checkLastListEmpty tmpState.captured then
                                            tmpState.captured
                                        else 
                                            List.append tmpState.captured []
                                    manageIteration { tmpState &  captured : captureUpdateOpen }  curState
      
                                else if matchThis.pattern.token == CaptureClose then
                                    tmpState = {toUpdateState & current : List.dropFirst toUpdateState.current }    
                                    manageIteration { tmpState &   captured : List.append tmpState.captured [] } curState
                                else

                                    updatedState = matchThis.state
                                    current = List.dropFirst  updatedState.current
                                    #   !!!!!!!!  List.isEmpty current  crashes  here  
                                    #dbg  (List.isEmpty current == Bool.true )
                                    updatedCapture =
                                        if matchThis.pattern.capture == Bool.true then
                                            when List.last updatedState.captured is
                                                Ok lst -> 
                                                    List.dropLast updatedState.captured
                                                    |> List.append (List.append lst utf)
                                                Err _ ->
                                                    updatedState.captured    
                                        else
                                            updatedState.captured
                                    
                                    when matchUtf utf  matchThis.pattern is 
                                        Consume updatedToken ->
                                            if List.len current == 0 then
                                                #{ updatedState & matched : Str.concat updatedState.matched utf, result : Bool.true  }
                                                List.append curState { regex : updatedState.regex, matched : List.append updatedState.matched  utf, current : current, result : Bool.true , missed : updatedState.missed, left : [], captured : updatedCapture, meta : updatedState.meta}
                                            else 
                                                #{ updatedState & matched : Str.concat updatedState.matched utf, current : List.dropFirst  updatedState.current }
                                                List.append curState { regex : updatedState.regex, matched : List.append updatedState.matched  utf, current : current, result : updatedState.result ,missed  : updatedState.missed, left : [], captured : updatedCapture, meta : updatedState.meta}
                                        NoMatch _ ->
                                                #{ updatedState & current : updatedState.regex, matched : "" } )
                                                noMatchCaptureUpdate =
                                                    if checkLastListEmpty updatedState.captured then
                                                        updatedState.captured
                                                    else 
                                                        List.dropLast updatedState.captured 
                                                List.append curState { regex : updatedState.regex, matched : [], current : updatedState.regex, result : updatedState.result , missed : List.append updatedState.missed utf, left : [], captured : noMatchCaptureUpdate, meta : updatedState.meta}
                                        _ -> curState  
                                )
                    manageIteration processedReg state ) 
        )
    List.walk complexSearch  { regex : reg, current : reg, matched : [], result : Bool.false, missed : [], left : [] , captured :[], meta : Inactive } ( \ state, parsResult -> 
        if state.result == Bool.true then
            if List.len parsResult.matched > List.len state.matched  then 
                parsResult
            else 
                state
        else 
            parsResult )
    

#  Ok [{ parsedResult: { current: [], left: [], matched: [46], missed: [], regex: [{ capture: Bool.false, n: 0, serie: Once, token: Dot }], result: Bool.true }, tag: Character }]

getRegexTokens = \ result  -> 
    when result.tag is 
        Character-> 
            when List.first result.parsedResult.matched is 
                Ok  matched  -> Ok [(createToken  ( Character matched )  Once Bool.false )]
                Err  _  -> Err "character  tag problem"
            
        _ -> Err "wrong tag"


firstStagePatterns = [
        { tag : Dot, str : "."},
        { tag : CaptureOpen, str : "("},
        { tag : CaptureClose, str : ")"},
        { tag : AtLeastOne, str : "+" }]
        
secondStagePatterns = []    


regexCreationStage = \ inPatterns, ignitionPatterns ->

    regPatterns = 
        List.walk inPatterns (Ok []) ( \ state, pat->
            when state is 
                Ok patterns -> 
                    when evalRegex (Str.toUtf8  pat.str ) ignitionPatterns [] is 
                        Ok result ->
                            if (List.len result == 1 ) then
                                when List.first result is 
                                    Ok  elem  ->
                                        when  getRegexTokens elem is 
                                            Ok tokens -> 
                                                workaround = 
                                                    List.walk  tokens [] ( \ workState, token -> 
                                                        List.append workState (createToken token.token token.serie  token.capture) ) 
                                                 
                                                
                                                # !!!!!!!!!!! crashes  here Ok [{ tag : pat.tag, tokens : tokens }]
                                                Ok (List.append patterns { tag : pat.tag, tokens : workaround } )    
                                                
                                            Err message -> Err message  

                                    Err _ -> Err "can't be here"                
                            else 
                                Err "parsing regex keys problem"

                        Err Empty ->  Err "Empty" 
                        Err NoTokens ->  Err "NoTokens"
                        Err PriorityListErr ->  Err "PriorityListErr"
                Err message ->  Err message )
    when regPatterns is 
        Ok patterns ->
            Ok ( List.concat  ignitionPatterns  patterns )
        Err  message  -> Err  message  
    
    
stagesCreationRegex  = \ _param -> 
    regexCreationStage firstStagePatterns regexSeedPattern
    #stage1  = regexCreationStage firstStagePatterns regexSeedPattern
    #when stage1 is 
    #    Ok stage1Pat -> 
    #        regexCreationStage secondStagePatterns stage1Pat
                
    #    Err message -> 
    #        Err message


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
                                                        when lastOnChain.token is 
                                                            CaptureClose -> 
                                                                List.append chainLst token            
                                                            _ -> 
                                                                List.dropLast chainLst
                                                                |> List.append  (createToken (Sequence ( modifLastInChain chain token))  Once Bool.false)
                                                    Err _ ->
                                                        List.dropLast chainLst
                                                        |> List.append  (createToken (Sequence ( modifLastInChain chain token))  Once Bool.false)
                                            _ ->  
                                                List.append chainLst token                  
                                    Err _ ->
                                        List.append chainLst token
                                        
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
                                    Ok { lst : modifLastInChain state.lst (createToken (Sequence [(createToken CaptureOpen Once Bool.false)]) Once Bool.false), capture : Bool.true}
                                CaptureClose ->
                                    Ok { lst : modifLastInChain state.lst (createToken CaptureClose Once Bool.false), capture : Bool.false }
                                AtLeastOne ->
                                    when List.last state.lst is
                                        Ok elem ->
                                            updatedLst = 
                                                List.dropLast state.lst
                                                |> List.append  {elem & serie : AtLeastOne }
                                              
                                            Ok { state &  lst : updatedLst }
                                        Err _ -> Err "Wrong usage of + in pattern"  
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
    
    
#parseStr \ str, pattern ->
#    regexCapable = regexCreationStage1  _
#    reg  = regexCreationStage2    pattern  regexCapable
    
#    checkMatching str reg  ->
# create  patterns  and  than  parse  string with it     



main =  
 
    stage1  = stagesCreationRegex []
    kwas = 
        when stage1 is 
            Ok stage1Pat -> 
                regexCreationStage2 "ds(.)r" stage1Pat  []
                
            Err message -> 
                Err message
    dbg  "kwas"
    zenon =
        when kwas  is 
            Ok pat -> 
                Ok (checkMatching (Str.toUtf8  "ds4rs" ) pat )
                    
            Err message -> 
                Err message          
    rama =
        when zenon is 
            Ok val ->
                
                dbg  val.result 
                4
            Err _ ->
                4 
    Stdout.line "adfasfsa"
    
    

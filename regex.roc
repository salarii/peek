    app "reg"

    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

# simplified, one level capture, nothing more needed  in context of peek (at least for now ), it is just easy this way 
priorities =
    Dict.empty {}
    |> Dict.insert Empty -1
    |> Dict.insert Character 0
    |> Dict.insert Dot 1
    |> Dict.insert CaptureOpen 2
    |> Dict.insert CaptureClose 3
    |> Dict.insert AtLeastOne 4
    |> Sequence 5

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

createToken = \ token, serie, n, capture ->
    when serie is
        NTimes ->
            { token :token,  n : n, serie : serie, capture : capture  } 
        _ ->
            { token :token,  n : 0, serie : serie, capture : capture  }

regexSeedPattern = [
    { tag : Character, tokens : [ createToken  Dot  Once 0 Bool.false ] } ]



evalRegex = \ utfLst, patterns, regex ->

    if List.isEmpty utfLst then
        Ok regex
        #Err NoTokens
        #[{ tag : Dot, parsedResult : { regex : [], current : [], matched : [], result : Bool.false } }]
    else
        List.walk patterns [] ( \ state, pattern ->
            out = checkMatching utfLst pattern.tokens
            if out.result == Bool.true  && List.isEmpty out.missed then
                List.append state { tag : pattern.tag, parsedResult : out } 
            else
                state )
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
    
    matchSerie = ( \ utf, tokenMeta ->
        result = matchStr utf tokenMeta.token
        when tokenMeta.serie is
            AtLeastOne ->
                when result is
                    Consume -> Keep { tokenMeta & n : tokenMeta.n + 1 }
                    NoMatch ->
                        if tokenMeta.n == 0 then
                            NoMatch tokenMeta 
                        else
                            Consume { tokenMeta & n : tokenMeta.n - 1 }
                     #{ tokenMeta & result : NoMatch }
            ZeroOrMore ->
                when result is
                    Consume -> Keep  tokenMeta 
                    NoMatch -> Consume tokenMeta 
            NTimes -> 
                when result is
                    Consume ->
                        if tokenMeta.n == 1 then
                            Consume  tokenMeta 
                        else
                            Keep { tokenMeta & n : tokenMeta.n - 1}
                    NoMatch -> NoMatch  tokenMeta 
            Once ->
                when result is
                    Consume -> Consume tokenMeta 
                    NoMatch -> NoMatch tokenMeta 
    )
    updateRegex = (\regex ->    
        regex ) 
    
    getFirstPat = (\  state  ->  
        when List.first state.current is
            Ok pat -> 
                { pattern : pat , state : state } 
            #Err _ ->  getFirstPat  { state & current : state.regex } )
            Err _ ->  getFirstPat  { regex : state.regex, current : state.regex, matched : state.matched, result : state.result, missed : state.missed , left : state.left, captured : state.captured} )

    #reg = 
    #    List.walk regInitial { reg : [], updatedCapture : Bool.false} ( \ state, tok ->
    #        if tok.token == CaptureOpen then
    #            {state & reg : List.append  state.reg tok,updatedCapture : Bool.true }
    #        else if tok.token == CaptureClose then
    #            {state & reg : List.append  state.reg tok,updatedCapture : Bool.false }
    #        else
    #            {state & reg : List.append  state.reg {tok & capture : state.updatedCapture  }  } )
    #    |> ( \ composite -> composite.reg )

    checkLastListEmpty = (\ listOfLists  ->
            when List.last listOfLists is
                Ok lst -> 
                    List.isEmpty lst
                Err _ ->
                    Bool.false  
    
        )

    complexSearch = 
        List.walk utfLst [{ regex : reg, current : reg, matched : [], result : Bool.false, missed : [], left : [] , captured : []}]  ( \ outState, utf ->
            updatedStates = updateRegex outState 
            List.walk updatedStates [] ( \ state, processedReg ->   
                if processedReg.result == Bool.true then
                    List.append state { regex : processedReg.regex, current : processedReg.current, matched : processedReg.matched, result : processedReg.result, missed : processedReg.missed , left : List.append  processedReg.left utf, captured : processedReg.captured } 
                else
                    # pattern  current captured
                    matchThis =
                        getFirstPat processedReg
                        |> ( \ tok ->
                            toUpdateState = tok.state  
                            if tok.pattern.token == CaptureOpen then
                                
                                tmp = getFirstPat {toUpdateState & current : List.dropFirst toUpdateState.current } 
                                tmpState = tmp.state
                                captureUpdateOpen = 
                                    if checkLastListEmpty tmpState.captured then
                                        tmpState.captured
                                    else 
                                        List.append tmpState.captured []
                                { tmp & state : { tmpState &   captured : captureUpdateOpen } }
                            else if tok.pattern.token == CaptureClose then
                                tmp = getFirstPat {toUpdateState & current : List.dropFirst toUpdateState.current } 
                                tmpState = tmp.state
                                { tmp & state : { tmpState &   captured : List.append tmpState.captured [] } }
                            else
                                tok )

                    updatedState = matchThis.state
                    current = List.dropFirst  updatedState.current
                    dbg  current
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
                        
                    
                    when matchSerie utf  matchThis.pattern is 
                        Consume updatedToken ->
                            
                            if List.isEmpty current == Bool.true then
                                #{ updatedState & matched : Str.concat updatedState.matched utf, result : Bool.true  }
                                List.append state { regex : updatedState.regex, matched : List.append updatedState.matched  utf, current : current, result : Bool.true , missed : updatedState.missed, left : [], captured : updatedCapture}
                            else 
                                #{ updatedState & matched : Str.concat updatedState.matched utf, current : List.dropFirst  updatedState.current }
                                List.append state { regex : updatedState.regex, matched : List.append updatedState.matched  utf, current : current, result : updatedState.result ,missed  : updatedState.missed, left : [], captured : updatedCapture}
                        Keep updatedToken ->

                                List.append state { regex : updatedState.regex, matched : List.append updatedState.matched  utf, current : current, result : updatedState.result ,missed  : updatedState.missed, left : [], captured : updatedCapture}
                        NoMatch _ ->   
                                #{ updatedState & current : updatedState.regex, matched : "" } )
                                noMatchCaptureUpdate =
                                    if checkLastListEmpty updatedState.captured then
                                        updatedState.captured
                                    else 
                                        List.dropLast updatedState.captured 
                                List.append state { regex : updatedState.regex, matched : [], current : updatedState.regex, result : updatedState.result , missed : List.append updatedState.missed utf, left : [], captured : noMatchCaptureUpdate} ))

    List.walk complexSearch  { regex : reg, current : reg, matched : [], result : Bool.false, missed : [], left : [] , captured :[] } ( \ state, parsResult -> 
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
                Ok  matched  -> Ok [(createToken  ( Character matched )  Once 0 Bool.false )]
                Err  _  -> Err "character  tag problem"
            
        _ -> Err "wrong tag"


firstStagePatterns = [
        { tag : Dot, str : "."},
        #{ tag : CaptureOpen, str : "("},
        #{ tag : CaptureClose, str : ")"},
        { tag : AtLeastOne, str : "+" }]
        
secondStagePatterns = [
        {tag : Sequence, str : "(.+"} 
    ]    


regexCreationStage = \ inPatterns, ignitionPatterns ->

    regPatterns = List.walk inPatterns (Ok []) ( \ state, pat->
        when state is 
            Ok patterns -> 
                when evalRegex (Str.toUtf8  pat.str ) ignitionPatterns [] is 
                    Ok result ->
                        if (List.len result == 1 ) then
                            when List.first result is 
                                Ok  elem  ->
                                    when  getRegexTokens elem is 
                                        Ok tokens -> 
                                            Ok (List.append patterns { tag : pat.tag, tokens :   tokens } )    
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
            Ok ( List.concat  regexSeedPattern  patterns )
        Err  message  -> Err  message  
    
    
StagesCreationRegex  = \ _param -> 
    stage1  = regexCreationStage firstStagePatterns regexSeedPattern
    when stage1 is 
        Ok stage1Pat -> 
            regexCreationStage secondStagePatterns stage1Pat
                
        Err message -> 
            Err message


regexCreationStage2  = \ str, patterns, currReg ->
    ( evalRegex (Str.toUtf8  str) patterns currReg )
    |> ( \ resultSet -> 
        when resultSet is
            Ok  results ->
                List.walk results (Ok { lst : [] , capture : Bool.false }) ( \ outState, result  ->
                    when outState is 
                        Err message -> Err message 
                        Ok state ->                    
                            when  result.tag is 
                                Character ->
                                    when List.first result.parsedResult.matched is 
                                        Ok  matched  ->
                                            Ok { state &  lst : List.append state.lst  (createToken (Character matched) Once 0 state.capture) }
                                        Err _ -> Err "parser  match problem"
                                    
                                Dot ->
                                    Ok { state &  lst : List.append state.lst (createToken Dot Once 0 state.capture )  }
                                CaptureOpen ->
                                    Ok { lst : List.append state.lst (createToken CaptureOpen Once 0 Bool.false), capture : Bool.true}
                                CaptureClose ->
                                    Ok { lst : List.append state.lst (createToken CaptureClose Once 0 Bool.false), capture : Bool.false }
                                AtLeastOne ->
                                    when List.last state.lst is
                                        Ok elem ->
                                            updatedLst = 
                                                List.dropLast state.lst
                                                |> List.append  {elem & serie : AtLeastOne }
                                              
                                            Ok { state &  lst : updatedLst }
                                        Err _ -> Err "Wrong usage of + in pattern"  
                                Sequence -> 
                                    List.dropFirst result.parsedResult.matched
                                    |> List.reverse 
                                    |> checkMatching (Str.toUtf8  ")" pat
                                    |> ( \ resultMatchingClose ->
                                        List.dropFirst resultMatchingClose.matched
                                        |> List.reverse
                                        |>  evalRegex  patterns []
                                        |> ( \ internalRegResult ->
                                            when internalRegResult is 
                                                Ok interResult ->
                                                    strLeft = List.reverse resultMatchingClose.left
                                                    regexCreationStage2  strLeft  patterns state.lst
                                                
                                                Err Empty ->  Err "Empty" 
                                                Err NoTokens ->  Err "NoTokens"
                                                Err PriorityListErr ->  Err "PriorityListErr" ) 

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
 
    stage1  = StagesCreationRegex []
    kwas = 
        when stage1 is 
            Ok stage1Pat -> 
                regexCreationStage2 "ds(.)d" stage1Pat  []
                
            Err message -> 
                Err message
    zenon =
        when kwas  is 
            Ok pat -> 
                Ok (checkMatching (Str.toUtf8  "ds4ds" ) pat )
                    
            Err message -> 
                Err message          

    Stdout.line "adfasfsa"
    
    

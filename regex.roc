    app "reg"

    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf


priorities =
    Dict.empty {}
    |> Dict.insert Empty -1
    |> Dict.insert Character 0
    |> Dict.insert Dot 1


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

createToken = \ token, serie, n ->
    when serie is
        NTimes ->
            { token :token,  n : n, serie : serie, capture : Bool.false  } 
        _ ->
            { token :token,  n : 0, serie : serie, capture : Bool.false  }

regexSeedPattern = [
    #{ tag : Dot, tokens : [ createToken ( Character (charToUtf ".")) Once 0 ] },
    { tag : Character, tokens : [ createToken  Dot  Once 0 ] } ]
    #{ tag : MatchOpen, tokens : [ createToken ( Character (charToUtf "(")) Once 0 ] },
    #{ tag : MatchClose, tokens : [ createToken ( Character (charToUtf ")")) Once 0 ] },
    #{ tag : MatchClose, tokens : [ createToken ( Character (charToUtf ")")) Once 0 ] } ]
    
    #{ tag : Character, tokens : [ createToken  (Character (charToUtf "\")) Once 0, createToken  (Character (charToUtf ".")) Once 0 ] } ]


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
    
    
    getFirstPat = (\  state  ->  
        when List.first state.current is
            Ok pat -> 
                { pattern : pat , state : state } 
            #Err _ ->  getFirstPat  { state & current : state.regex } )
            Err _ ->  getFirstPat  { regex : state.regex, current : state.regex, matched : state.matched, result : state.result, missed : state.missed , left : state.left } )
            
    List.walk utfLst { regex : reg, current : reg, matched : [], result : Bool.false, missed : [], left : [] }  ( \ state, utf ->
        if state.result == Bool.true then
            { regex : state.regex, current : state.current, matched : state.matched, result : state.result, missed : state.missed , left : List.append  state.left utf } 
        else
            matchThis = getFirstPat state
            updatedState = matchThis.state
            when matchSerie utf  matchThis.pattern is 
                Consume updatedToken ->
                    current = List.dropFirst  updatedState.current
                    if List.isEmpty current == Bool.true then
                        #{ updatedState & matched : Str.concat updatedState.matched utf, result : Bool.true  }
                        { regex : updatedState.regex, matched : List.append updatedState.matched  utf, current : current, result : Bool.true , missed : [], left : []}
                    else 
                        #{ updatedState & matched : Str.concat updatedState.matched utf, current : List.dropFirst  updatedState.current }
                        { regex : updatedState.regex, matched : List.append updatedState.matched  utf, current : current, result : updatedState.result , missed : [], left : []}
                Keep updatedToken ->
                        current = 
                            List.dropFirst  updatedState.current
                            |> List.prepend  updatedToken
                        { regex : updatedState.regex, matched : List.append updatedState.matched  utf, current : current, result : updatedState.result , missed : [], left : []}
                NoMatch _ ->   
                        #{ updatedState & current : updatedState.regex, matched : "" } )
                        { regex : updatedState.regex, matched : [], current : updatedState.regex, result : updatedState.result , missed : [], left : []} )

#  Ok [{ parsedResult: { current: [], left: [], matched: [46], missed: [], regex: [{ capture: Bool.false, n: 0, serie: Once, token: Dot }], result: Bool.true }, tag: Character }]

getRegexTokens = \ result  -> 
    when result.tag is 
        Character-> 
            when List.first result.parsedResult.matched is 
                Ok  matched  -> Ok [(createToken  ( Character matched )  Once 0 )]
                Err  _  -> Err "character  tag problem"
            
        _ -> Err "wrong tag"



regexCreationStage1 = \ _seed ->
    #createPatterns = ( \ tag, matchingResult ->
    #    when tag is 
    #        Character ->
    #            { tag : Character, tokens : [ createToken  Dot  Once 0 ] } 
            #{ tag : pattern.tag, parsedResult : out }  )
    #    )
         
    firstStagePatterns = [
        { tag : Dot, str : "."} ]
        
    #List.walk     
    
    regPatterns = List.walk firstStagePatterns (Ok []) ( \ state, pat->
        when state is 
            Ok patterns -> 
                when evalRegex (Str.toUtf8  pat.str ) regexSeedPattern [] is 
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
    
    

#regexCreationStage2  = \ str, patterns ->
#    evalRegex (Str.toUtf8  ".") patterns []
    
    
#parseStr \ str, pattern ->
#    regexCapable = regexCreationStage1  _
#    reg  = regexCreationStage2    pattern  regexCapable
    
#    checkMatching str reg  ->
# create  patterns  and  than  parse  string with it     



main =  
    #dbg checkMatching "w"  ([ createToken  Dot  Once 0 ])  
    dbg  regexCreationStage1 []
    #dbg evalRegex (Str.toUtf8  ".") regexSeedPattern []
    #dbg checkMatching "ds4ds"  [NotInRange [ charToUtf "e" ,charToUtf "d"],NoDigit,Dot,Character (charToUtf "s")] 
    
    Stdout.line "adfasfsa"
    
    

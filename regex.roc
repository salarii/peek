    app "reg"

    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

# I highly prioritized implementation easines over performance (whenever there is a design choce) 
# in  general more I dig into it, more and more corner cases come to the surface. I think I don't need to deal with all of them. I will limit myself to use cases vailid in context of peek app 
# dbg crashing left and right on some of more complex object


firstStagePatterns = [
        { tag : Dot, str : "."},
        { tag : CaptureOpen, str : "("},
        { tag : CaptureClose, str : ")"},
        { tag : AtLeastOne, str : "+" },
        { tag : Separator, str : "|" },
        { tag : Digit, str : "\\d" },
        { tag : NoDigit, str : "\\D" },
        { tag : Alphanumeric, str : "\\w" },
        { tag : NoAlphanumeric, str : "\\W" },
        { tag : Whitespace, str : "\\s" },
        { tag : NoWhitespace, str : "\\S" }]
        
        
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
     { regex : regex, current : regex, matched : [], result : Bool.false, missed : [], left : [] , captured : treeBase, meta : meta, strict : No } 


checkMatchingValidity = \ matchingResult ->
    when matchingResult.strict is 
        No -> Bool.true 
        Front -> List.isEmpty  matchingResult.missed
        Back -> List.isEmpty  matchingResult.left 
        Both -> (List.isEmpty  matchingResult.missed) && (List.isEmpty  matchingResult.left)

walkUntil : 
    List elem, 
    state,     
    (state, 
    elem
    -> 
        [
            Continue state,
            Break state
        ])
    -> state



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
            if out.result == Bool.true   && List.isEmpty out.missed then
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
                                        (List.dropFirst regItem.current 1)
                                        |> List.prepend { pat & serie : ZeroOrMore }
                                        
                                    List.concat chain changeFront
                                    |> (\ updatedCurrent ->  List.append state {regItem & current : updatedCurrent} )  
                                ZeroOrMore ->

                                    List.append state {regItem & current : (List.dropFirst regItem.current 1)}  
                                    |> List.append 
                                        (List.concat chain (List.dropFirst regItem.current 1)
                                        |> (\ updatedCurrent ->  {regItem & current : updatedCurrent, meta : Active} ))
                                NTimes cnt -> 
                                    concatIter = (\ n , lst, stored ->
                                        if n == 0 then 
                                            stored
                                        else 
                                            concatIter (n-1) lst (List.concat lst stored  ))
                                    
                                    List.concat (concatIter cnt chain  [] ) (List.dropFirst regItem.current 1)
                                    |> (\ updatedCurrent ->  List.append state {regItem & current : updatedCurrent} )  
                                Once ->
                                    List.concat chain (List.dropFirst regItem.current 1)
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
                                toUpdateState = matchThis.state  
                                if matchThis.pattern.token == CaptureOpen then
                                    tmpState = {toUpdateState & current : List.dropFirst toUpdateState.current 1}

                                    manageIteration { tmpState &  captured : (composeMatchedStructure tmpState.captured  0 CaptureOpen) }  curState
                                    
                                else if matchThis.pattern.token == CaptureClose then
                                    
                                    tmpState = {toUpdateState & current : List.dropFirst toUpdateState.current 1}    
                                    manageIteration { tmpState &   captured : composeMatchedStructure tmpState.captured  0 CaptureClose } curState
                                else
                                    
                                    updatedState = matchThis.state
                                    current = List.dropFirst  updatedState.current  1
                                    #   BUG  this  crashes 
                                    #ppp = List.isEmpty current == Bool.true 
                                    
                                    updatedCapture =
                                        if matchThis.pattern.capture == Bool.true then
                                            when  changeValue updatedState.captured 0 utf  is 
                                                Ok updatedTree -> updatedTree
                                                Err _ ->  updatedState.captured
                                        else
                                            updatedState.captured
                                    
                                    when matchUtf utf  matchThis.pattern is 
                                        Consume updatedToken ->
                                            if List.len current == 0 then
                                                List.append curState { updatedState &  matched : List.append updatedState.matched  utf, current : current, result : Bool.true, left : [], captured : updatedCapture}
                                            else
                                                List.append curState { updatedState & matched : List.append updatedState.matched  utf, current : current, left : [], captured : updatedCapture} 
                                        NoMatch _ ->
                                                
                                                updateMissed = List.concat updatedState.missed  updatedState.matched
                                                # BUG this line 
                                                #List.append curState { updatedState &  matched : [], current : updatedState.regex, missed : List.append updateMissed utf, left : [], captured : treeBase}
                                                List.append curState { regex : updatedState.regex, current : updatedState.regex, matched : [], result : updatedState.result, missed : updatedState.missed, left : updatedState.left, captured : treeBase, meta : updatedState.meta, strict : updatedState.strict }
                                                
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
                Ok  matched  -> Ok [(createToken  ( Character matched )  Once Bool.false )]
                Err  _  -> Err "character  tag problem"
            
        _ -> Err "wrong tag"


 

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
                                                        when token.token is 
                                                            CaptureClose -> 
                                                                List.append chainLst token            
                                                            _ -> 
                                                                modifyLastInList chainLst (createToken (Sequence ( modifLastInChain chain token))  Once Bool.false)

                                                    Err _ ->
                                                        modifyLastInList  chainLst  (createToken (Sequence ( modifLastInChain chain token))  Once Bool.false)
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
                                AtLeastOne ->
                                    
                                    # this exist because of some failures in design
                                    omitCaptureEnd = ( \ inLst -> 
                                        when List.last inLst is
                                            Ok elem ->
                                                when elem.token is 
                                                    CaptureClose ->
                                                        when omitCaptureEnd ( List.dropLast inLst  1) is 
                                                            Ok updatedLst ->
                                                                Ok (List.append updatedLst elem)
                                                            Err message -> Err message 
                                                    _ ->
                                                        Ok ( modifyLastInList inLst {elem & serie : AtLeastOne } )
                                                
                                            Err _ -> Err "Wrong usage of + in pattern"        
                                        ) 
                                    when omitCaptureEnd state.lst is 
                                        Ok newLst -> Ok { state &  lst : newLst }
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
    
    

    
#    checkMatching str reg  ->
# create  patterns  and  than  parse  string with it     

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
            _ -> 
                "\n unknown token "
))

availableRegex = stagesCreationRegex [] 

# maybe at some point add some additional error handling ??


# to be replaced 


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

    modifyActive tree id modify
                    
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


    
main =
    pattern = "aaaa"
    pp =  parseStr  "sss"   "a"
    # BUG  as usuall problem with dbg     
    #dbg  pp 
    






    #outStr = 
    #    when (parseStr  "dssrr"   "ds(.)r") is 
    #        Ok result -> 
    #            if result.result  == Bool.true  then
    #                "no match found"
    #            else 
                    # it is good to see current to investigate what actually matched  
    #                printMe result.current
                
    #        Err message  -> message  
   # l = when  availableRegex  is 
    #    Ok  rrr ->
                #dbg  List.len rrr
     #           s = List.walk  rrr  1 ( \ state , cyk  -> 
                            #dbg  cyk.tag
                            #dbg  (printMe cyk.tokens )
      #                      2
                        
       #         )  
       #         2
       # Err _ ->  2
            
    
    Stdout.line "outStr"
    
    

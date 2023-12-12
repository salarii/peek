app "command"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br"
    }
    #exposes []
    imports [
        pf.Stdout,
        pf.Stdin,
        Regex,
    ]
    provides [main] to pf


RecoverRegexTagsType : [ Empty, Character, Dot, CaptureOpen, CaptureClose, AtLeastOne, ZeroOrMore, 
    Optional, Separator, Digit, NoDigit, Alphanumeric,NoAlphanumeric,
    Whitespace, NoWhitespace, BackSlash, Repetition, RangeRepetition, Except, Only]


TokenPerRegexType : {tag : RecoverRegexTagsType, tokens : List TokenType} 

RecoverPatternType : { tag : RecoverRegexTagsType, str : Str }

TreeNodeType : {locked : Bool, children : List I32, value : List U8}

TreeType : { cnt : I32, content : Dict I32  TreeNodeType }



SerieType : [ AtLeastOne, ZeroOrMore, MNTimes I32 I32, NoMorethan I32, NTimes I32, Once ] 

MetaType :  [Inactive, Active]

StrictType : [ No, Front, Back, Both ]

SearchRegexTagsType : [ Dot, CaptureClose, CaptureOpen, ReverseLimitRanges ( List { left : U8, right : U8 } ),
            Separator,  CharacterSet (List U8), NotInCharacterSet (List U8), 
            Except (List (SearchRegexTagsType)), Character U8, Digit, NoDigit, LimitRanges ( List { left : U8, right : U8 } ),
            Sequence (List  { token :SearchRegexTagsType, serie : SerieType , capture : Bool  } ) ]  


TokenType : { token :SearchRegexTagsType , serie : SerieType , capture : Bool  }


ParsingResultType : { regex : List TokenType, current : List TokenType, matched : List U8, result : Bool,
                missed : List U8, left : List U8, captured : TreeType, meta : MetaType, strict : StrictType }



firstStagePatterns :  List  RecoverPatternType
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

aux1Patt : Str
aux1Patt =  "T(((.+)))T"

secondStagePatterns: List  RecoverPatternType
secondStagePatterns =  [
    { tag : BackSlash, str : "\\T.?{}^*T" },  # " #<- this artifact is serves for visual studio code 
    { tag : Repetition, str : "{(\\d)+}"},   #"   
    { tag : RangeRepetition, str : "{(\\d)+,(\\d)+}" }, #"
    { tag : Except, str : "[^(((.)-(.))|((.))|((\\T.?{}^*T)))+]" }, #"
    { tag : Only, str : "[(((.)-(.))|((.))|((\\T.?{}^*T)))+]" } #"
    ]  # "

    
priorities : Dict  RecoverRegexTagsType (Num a)    
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
  
  
  
getPrioToken : List {tag : RecoverRegexTagsType,parsedResult : ParsingResultType} ->  Result {tag : RecoverRegexTagsType,parsedResult : ParsingResultType} [Empty, NoTokens, PriorityListErr]
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
        
    
charToUtf : Str -> U8
charToUtf = \ char ->
    Str.toUtf8 char
    |> List.first
    |> Result.withDefault 0

emptyNode : TreeNodeType
emptyNode = { locked : Bool.false, children :  [], value : [] }

treeBase : TreeType
treeBase = 
    {cnt : 0, content : Dict.empty {} }
    |> addElement  0 emptyNode

changeValue : TreeType, I32, U8 ->  TreeType
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

addElement : TreeType, I32, TreeNodeType ->  TreeType
addElement = \ tree, parentId ,node -> 
    when Dict.get tree.content parentId  is 
        Ok element -> 
            updatedContent = 
                Dict.remove tree.content parentId
                |> Dict.insert parentId {element  & children : (List.append element.children tree.cnt )}
            
            { cnt : tree.cnt + 1, content : (Dict.insert updatedContent  tree.cnt node )  }          
        Err _ -> { cnt : tree.cnt + 1, content : Dict.insert tree.content  tree.cnt node }

modifyActive : TreeType, I32, (I32, TreeType -> Result TreeType Str)  -> Result  TreeType  Str 
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


changeElement : TreeType, I32, TreeNodeType ->  TreeType
changeElement = \ tree, id ,node -> 
    when Dict.get tree.content id  is 
        Ok element -> 
            updatedContent = 
                Dict.remove tree.content id
                |> Dict.insert id node
            
            { tree & content : updatedContent }          
        Err _ -> { cnt : tree.cnt + 1, content : Dict.insert tree.content  tree.cnt node }
    
composeMatchedStructure :  TreeType, I32, [CaptureOpen, CaptureClose] -> TreeType 
composeMatchedStructure = \ tree, id, tag ->
    
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
    
getDirectly : TreeType, I32 -> Result (List U8)  Str
getDirectly = \ tree, id  ->
    when Dict.get tree.content id  is 
        Ok node ->
            Ok node.value  
        Err _ -> Err  "no such value"

getDirectlyChildrenCnt : TreeType, I32 -> Result (Nat)  Str
getDirectlyChildrenCnt = \ tree, id  ->
    when Dict.get tree.content id  is 
        Ok node ->
            Ok (List.len node.children )  
        Err _ -> Err  "no such node"


createParsingRecord : List TokenType, MetaType, StrictType -> ParsingResultType  
createParsingRecord = \ regex, meta, strict ->
    { regex : regex, current : regex, matched : [], result : Bool.false, missed : [], left : [] , captured : treeBase, meta : meta, strict : strict }

createToken : SearchRegexTagsType, SerieType, Bool -> TokenType
createToken = \ token, serie, capture ->
    { token :token, serie : serie, capture : capture  }

regexSeedPattern : List ( TokenPerRegexType )
regexSeedPattern = [
    { tag : Character, tokens : [ createToken  Dot  Once Bool.false ] } ]

modifyLastInList : List  a, a -> List  a
modifyLastInList = \ lst, elem ->
    List.dropLast lst 1
    |> List.append elem

splitChainOnSeparators : List TokenType,List TokenType -> List( List TokenType )
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


evalRegex : List U8, List TokenPerRegexType, List {tag : RecoverRegexTagsType,parsedResult : ParsingResultType}  -> Result ( List {tag : RecoverRegexTagsType,parsedResult : ParsingResultType} ) [Empty, NoTokens, PriorityListErr]
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


checkMatching : List U8, List TokenType -> ParsingResultType
checkMatching = \ utfLst, reg  -> 
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
                    when matchStr utf  token is
                #    when matchStrBugPrevent  utf  token is
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
    
    complexSearch : { missed : List U8,  parsing : List ParsingResultType } 
    complexSearch = 
        List.walk utfLst { missed : [] , parsing : [createParsingRecord regexStartEndSetting.updatedRegex Active  regexStartEndSetting.strict]}  ( \ outState, utf ->
            
            updatedStates = updateRegex outState.parsing 
            List.walk updatedStates [] ( \ state, processedReg ->   
                
                # it is  stupid  but without  this line it is crashing
                #a  =  printMe  processedReg.regex
                
                
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

printMe : List  TokenType -> Str
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
            Digit -> Str.concat inState "\nDigit"
            CharacterSet _ -> Str.concat inState "\nCharacter set"  
            LimitRanges _ -> Str.concat inState "\nLimit Ranges set"
            _ -> 
                "\n unknown token "
        )
)
checkMatchingValidity : ParsingResultType -> Bool
checkMatchingValidity = \ matchingResult ->
    when matchingResult.strict is 
        No -> Bool.true 
        Front ->
            List.isEmpty  matchingResult.missed
        Back -> List.isEmpty  matchingResult.left 
        Both -> (List.isEmpty  matchingResult.missed) && (List.isEmpty  matchingResult.left)


main = 
    #Stdout.write  clearScreenPat |> Task.await
    Stdout.write  "kapusta"





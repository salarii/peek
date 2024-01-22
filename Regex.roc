interface Regex
    exposes [
        parseStr,
        getValue,
        ParsingResultType,
        TokenType,
        regexMagic,
        MagicType,
        parseStrMagic,
        createParsingRecord]
    imports [Utils]


RecoverRegexTagsType : [ Empty, Character, Dot, CaptureOpen, CaptureClose, AtLeastOne, ZeroOrMore,
    Optional, Separator, Digit, NoDigit, Alphanumeric,NoAlphanumeric,
    Whitespace, NoWhitespace, BackSlash, Repetition, RangeRepetition, Except, Only, FromTo]


TokenPerRegexType : {tag : RecoverRegexTagsType, tokens : List TokenType}

RecoverPatternType : { tag : RecoverRegexTagsType, str : Str }

TreeNodeType : {locked : Bool, children : List I32, value : List U8}

TreeType : { cnt : I32, content : Dict I32  TreeNodeType }

SerieType : [ AtLeastOne, ZeroOrMore, MNTimes I32 I32, NoMorethan I32, NTimes I32, Once ]

MetaType :  [Inactive, Active]

StrictType : [ No, Front, Back, Both ]

SearchRegexTagsType : [ Dot, CaptureClose, CaptureOpen, ReverseLimitRanges ( List { left : U8, right : U8 } ),
            Separator,  CharacterSet (List U8), NotInCharacterSet (List U8), Only (List (SearchRegexTagsType)),
            Except (List (SearchRegexTagsType)), Character U8, Digit, NoDigit, LimitRanges ( List { left : U8, right : U8 } ),
            Sequence (List  { tag :SearchRegexTagsType, serie : SerieType , capture : Bool  } ) ]


TokenType : { tag :SearchRegexTagsType , serie : SerieType , capture : Bool  }


ParsingResultType : { regex : List TokenType, current : List TokenType, matched : List U8, matchFound : Bool,
                missed : List U8, left : List U8, captured : TreeType, meta : MetaType, strict : StrictType }


regexSeedPattern : List ( TokenPerRegexType )
regexSeedPattern = [
    { tag : Character, tokens : [ createToken  Dot  Once Bool.false ] } ]

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


secondStageHelperPatterns :  List  RecoverPatternType
secondStageHelperPatterns = [
    {tag : Except,  str : "H(.+)H" },
]

secondStagePatterns :  List  RecoverPatternType
secondStagePatterns = [
    {tag : Except, str : "[^(H]H+)]" },
    {tag : Only,  str : "[(H]H+)]" },
]

thirdStagePatterns: List  RecoverPatternType
thirdStagePatterns =  [
    { tag : BackSlash, str : "\\[.?{}()^*]" },  # " #<- this artifact is for visual studio code
    { tag : Repetition, str : "{(\\d)+}"},   #"
    { tag : RangeRepetition, str : "{(\\d)+,(\\d)+}" }, #"
    ]  # "

firstStageChains : Result (List TokenPerRegexType ) Str
firstStageChains = evalAndAppendChain firstStagePatterns (Ok regexSeedPattern) (Ok regexSeedPattern)


onlyExceptInternalPatterns : List  RecoverPatternType
onlyExceptInternalPatterns = [
    { tag : Character, str : "."},
    { tag : BackSlash, str : "\\[.?{}^*]"},  # " #<- this artifact is for visual studio code
    { tag : FromTo, str : "(.)-(.)"},
    ]

evalAndAppendChain : List  RecoverPatternType, Result (List TokenPerRegexType ) Str, Result (List TokenPerRegexType ) Str  ->  Result (List TokenPerRegexType ) Str
evalAndAppendChain = \ pattern, stageChain, onlyExceptChain ->
    when stageChain is
        Ok chain ->
            when createRegexFromData pattern stageChain onlyExceptChain is
                Ok newChainElem ->
                    Ok (List.concat chain newChainElem)
                Err message -> Err message
        Err _ -> Err  "chain stage broken, can't generate"

secondStageHelperChains : Result (List TokenPerRegexType ) Str
secondStageHelperChains = evalAndAppendChain secondStageHelperPatterns firstStageChains firstStageChains

secondStageChains : Result (List TokenPerRegexType ) Str
secondStageChains =  evalAndAppendChain  secondStagePatterns secondStageHelperChains (Ok regexSeedPattern)

thirdStageChains : Result (List TokenPerRegexType ) Str
thirdStageChains = evalAndAppendChain  thirdStagePatterns secondStageChains (Ok regexSeedPattern)

onlyExceptInternalChains : Result (List TokenPerRegexType ) Str
onlyExceptInternalChains = createRegexFromData onlyExceptInternalPatterns secondStageHelperChains (Ok regexSeedPattern)



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
    |> Dict.insert BackSlash 2
    |> Dict.insert Repetition 2
    |> Dict.insert FromTo 2
    |> Dict.insert RangeRepetition 2
    |> Dict.insert Only 3
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

    when modifyActive tree id modify Bool.true is
        Ok newTree -> newTree
        _ -> tree

cntLivesValues : TreeType, I32->  Nat
cntLivesValues = \ tree, id ->
    when Dict.get tree.content id is
        Ok node ->
            if List.isEmpty node.children == Bool.true then
                List.len node.value
            else
                List.walk node.children 0 ( \cnt, childId ->
                    cnt + cntLivesValues tree childId
                    )
        Err _ -> 0

addElement : TreeType, I32, TreeNodeType ->  TreeType
addElement = \ tree, parentId ,node ->
    when Dict.get tree.content parentId  is
        Ok element ->
            updatedContent =
                Dict.remove tree.content parentId
                |> Dict.insert parentId {element  & children : (List.append element.children tree.cnt )}

            { cnt : tree.cnt + 1, content : (Dict.insert updatedContent  tree.cnt node )  }
        Err _ -> { cnt : tree.cnt + 1, content : Dict.insert tree.content  tree.cnt node }

modifyActive : TreeType, I32, (I32, TreeType -> Result TreeType Str), Bool  -> Result  TreeType  Str
modifyActive = \ tree, headId, op, allLevels ->
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
                                    if allLevels == Bool.true then
                                        modifyActive (Result.withDefault (op headId tree) tree) nodeId op allLevels
                                    else
                                        modifyActive tree nodeId op allLevels
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

getValue : List Nat, I32, TreeType -> Result (List U8) Str
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

createRepetitionOutOfTree : TreeType -> Result SerieType Str
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
                                        when Str.toI32 strVal  is
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


mergeTokens : List TokenType -> Result (List SearchRegexTagsType ) Str
mergeTokens = \ lst ->
    List.walk lst (Ok {characterSet :CharacterSet [], limitRanges : LimitRanges [] } ) ( \ stateResult, token ->
            when  stateResult is
                Ok state ->
                    when token.tag is
                        Character char  ->
                            when state.characterSet is
                                CharacterSet chars ->
                                    Ok { state &  characterSet : CharacterSet ( List.append chars char ) }
                                _ -> Err  "problem in merging tokens"

                        LimitRanges  ranges ->
                            when ranges is
                                [ range ]  ->
                                    when state.limitRanges is
                                        LimitRanges inRanges ->
                                            Ok { state &  limitRanges : LimitRanges ( List.append inRanges range ) }
                                        _ -> Err  "problem in merging tokens"
                                _ -> Err  "problem in merging tokens"
                        _ ->  Err  "problem in merging tokens"
                Err message -> Err message
        )
        |> ( \ mergedResult ->
            when mergedResult is
                Ok merged  ->
                    CharacterSet chars = merged.characterSet
                    (when chars is
                        [] ->
                            []
                        _ ->
                            [ CharacterSet chars]
                            #[merged.characterSet]   <--- this  does  not work  I consider  it a bug
                    )
                    |> ( \ tokens ->
                        LimitRanges ranges = merged.limitRanges
                        when ranges is
                            [] ->
                                Ok tokens
                            _ ->
                                Ok (List.append tokens (LimitRanges ranges))
                            )
                Err message -> Err message
                )



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
                    modifyActive tree id addNewNode Bool.false
                else
                    Err  "internal logic error"
            CaptureClose ->
                modifyActive tree id lockNode Bool.false
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
    { regex : regex, current : regex, matched : [], matchFound : Bool.false, missed : [], left : [] , captured : treeBase, meta : meta, strict : strict }

createToken : SearchRegexTagsType, SerieType, Bool -> TokenType
createToken = \ tag, serie, capture ->
    { tag :tag, serie : serie, capture : capture  }


splitChainOnSeparators : List TokenType,List TokenType -> List( List TokenType )
splitChainOnSeparators = \ chain, inputLst ->
    when List.first chain is
        Ok elem ->
            when elem.tag is
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
                            Utils.modifyLastInList partialResult  (List.concat [elem] lst)
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
            if out.matchFound == Bool.true  && List.isEmpty out.missed then
                List.append state { tag : pattern.tag, parsedResult : out }
            else
                state
            )
        |> getPrioToken
        |> ( \ prioToken ->
            when prioToken is
                Ok token ->
                    evalRegex token.parsedResult.left patterns  ( List.append regex token )
                Err message ->  Err message  )


checkMatching : List U8, List TokenType -> ParsingResultType
checkMatching = \ utfLst, reg  ->


    matchStr : U8, SearchRegexTagsType -> [Consume, NoMatch]
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
            Digit ->
                when checkRange utf [{ left : 48, right : 57 }] is
                    Within -> Consume
                    Outside -> NoMatch
            NoDigit ->
                when checkRange utf [{ left : 48, right : 57 }] is
                    Within -> NoMatch
                    Outside -> Consume
            Except  tokens  ->
                List.walkUntil tokens Consume ( \  state , token  ->
                    when matchStr utf  token is
                        Consume -> Break  NoMatch
                        NoMatch -> Continue state
                    )
            Only  tokens  ->
                List.walkUntil tokens Consume ( \  state , token  ->
                    when matchStr utf  token is
                        Consume -> Continue state
                        NoMatch -> Break  NoMatch
                    )
            Character val ->
                if val == utf then
                    Consume
                else
                    NoMatch
            Dot ->
                Consume
            _ -> NoMatch

    matchUtf : U8, TokenType ->  [Consume TokenType, NoMatch TokenType]
    matchUtf = ( \ utf, tokenMeta ->
        result = matchStr utf tokenMeta.tag

        when result is
            Consume -> Consume tokenMeta
            NoMatch -> NoMatch tokenMeta
    )
    updateRegex : List ParsingResultType  ->  List ParsingResultType
    updateRegex = (\regex ->
        updateRegexItemInternal : List ParsingResultType, ParsingResultType ->  List ParsingResultType
        updateRegexItemInternal = ( \ state, regItem ->

                when List.first regItem.current is
                    Ok pat ->
                        concatIter = (\ n , lst, stored ->
                            if n == 0 then
                                stored
                            else
                                concatIter (n-1) lst (List.concat lst stored  ))
                        when pat.tag  is
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
            if regItem.meta == Active then
                updateRegexItemInternal state regItem
            else

                List.append  state regItem
            )
        )
    getFirstPat : ParsingResultType -> [Active {  pattern : TokenType , state : ParsingResultType }, Inactive ParsingResultType]
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
    regeneratePat : ParsingResultType ->[Old ParsingResultType, New ParsingResultType ]
    regeneratePat = (\  state  ->
        when List.first state.current is
            Ok pat ->
                Old state
            Err _ ->
                New {state & meta : Inactive }  )

    checkLastListEmpty : List (List a) -> Bool
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
    regexStartEndSetting : { updatedRegex : List TokenType, strict :  StrictType}
    regexStartEndSetting =
        when (List.first reg, List.last reg ) is
            (Ok  (  tokenFirst) , Ok (  tokenLast) ) ->
                when ( tokenFirst.tag, tokenLast.tag )  is
                    (Character  first,Character  last )  ->
                        if first == '^' && last == '\$' then  #
                            { updatedRegex :
                                List.dropFirst reg 1
                                |> List.dropLast 1,
                                strict : Both }
                        else if first == '^'  then
                            { updatedRegex : List.dropFirst reg 1, strict : Front }
                        else if last == '\$'  then
                            { updatedRegex : List.dropLast reg 1, strict : Back }
                        else
                            { updatedRegex : reg, strict : No }
                    (Character  first, _) ->
                        if first == '^'  then
                            { updatedRegex : List.dropFirst reg 1, strict : Front }
                        else
                            { updatedRegex : reg, strict : No }

                    (_, Character  last) ->
                        if last == '\$'  then
                            { updatedRegex : List.dropLast reg 1, strict : Back }
                        else
                            { updatedRegex : reg, strict : No }

                    _ ->
                        { updatedRegex : reg, strict : No }
            _ ->  { updatedRegex : reg, strict : No }

    push = 'a'
    complexSearch : { cnt : Nat, missed : List U8,  parsing : List ParsingResultType }
    complexSearch =
        #
        List.walk (List.append utfLst push)  { cnt : 1, missed : [] , parsing : [createParsingRecord regexStartEndSetting.updatedRegex Active  regexStartEndSetting.strict]}  ( \ outState, utf ->

            updatedStates = updateRegex outState.parsing


            List.walk updatedStates [] ( \ state, processedReg ->

                if processedReg.matchFound == Bool.true  then
                    if outState.cnt <= List.len utfLst then
                        List.append state { processedReg & left : List.append  processedReg.left utf}
                    else
                        List.append state processedReg
                else
                    manageIteration = ( \ inProcessedReg, curState ->

                        when getFirstPat inProcessedReg is
                            Inactive patternSet ->
                                if  outState.cnt > List.len utfLst then
                                    List.append curState { inProcessedReg & current : [], matchFound : Bool.true, left : [], meta : Inactive}
                                else
                                    List.append curState { inProcessedReg & current : [], matchFound : Bool.true, left : [utf], meta : Inactive}

                            Active matchThis ->
                                    updatedState = matchThis.state

                                    current = List.dropFirst  updatedState.current  1
                                    updatedCapture =
                                        if matchThis.pattern.capture == Bool.true then
                                            changeValue updatedState.captured 0 utf
                                        else
                                            updatedState.captured
                                    when matchUtf utf  matchThis.pattern is
                                        Consume updatedToken ->
                                            if outState.cnt > List.len utfLst then
                                                curState
                                            else
                                                if List.isEmpty current == Bool.true then
                                                    List.append curState { updatedState &  matched : List.append updatedState.matched  utf, current : [], matchFound : Bool.true, left : [], captured : updatedCapture, meta : Inactive}
                                                else
                                                    List.append curState { updatedState & matched : List.append updatedState.matched  utf, current : current, left : [], captured : updatedCapture}

                                        NoMatch _ ->
                                                curState

                                        _ -> curState
                                )
                    manageIteration processedReg state )
                    |> ( \ parsingSet  ->

                            updatedMissed = List.append outState.missed utf
                            parsingState = createParsingRecord regexStartEndSetting.updatedRegex Active  regexStartEndSetting.strict
                            {
                            cnt : outState.cnt + 1,
                            missed : updatedMissed,
                            parsing : List.append  parsingSet { parsingState & missed : updatedMissed}  }

                        )

        )
    List.walk complexSearch.parsing  (createParsingRecord reg Inactive  No) ( \ state, parsResult ->

        updatedParsResult  =  { parsResult &  matchFound :(checkMatchingValidity parsResult) && parsResult.matchFound  }

        if state.matchFound == Bool.true then
            if  updatedParsResult.matchFound == Bool.true &&
                List.len updatedParsResult.matched > List.len state.matched  then
                updatedParsResult
            else
                state
        else
            updatedParsResult )

printSerie : SerieType  -> Str
printSerie = ( \ serie ->
    when serie is
        AtLeastOne -> "\nAtLeastOne"
        ZeroOrMore -> "\nZeroOrMore"
        MNTimes _ _ -> "\nMNTimes"
        NoMorethan _ -> "\nNoMorethan"
        NTimes _ -> "\nNTimes"
        Once -> "\nOnce"
        _ ->  "\nUnknown serie"

)

printTag : SearchRegexTagsType -> Str
printTag = (  \ tag ->

        when  tag  is
            Character  val ->
                extractedChar =
                    when Str.fromUtf8 [val] is
                        Ok str -> str
                        Err _ -> "  some weird problem in  extracting character token "
                "\n"
                |> Str.concat  "character : "
                |> Str.concat  extractedChar
            Dot -> "\nDot"
            CaptureOpen ->
                "\nCapture  Open"
            CaptureClose ->
                "\nCapture  Close"
            Separator ->
                "\nSeparator"
            Sequence  array  ->
                "\nseq : -> "
                |> Str.concat (printTokens array)
                |> Str.concat "\nexit sequence  <- "
            Digit -> "\nDigit"
            NoDigit -> "\nNo Digit"
            CharacterSet _ ->
                "\nCharacter set"
            LimitRanges _ -> "\nLimit Ranges set"
            ReverseLimitRanges _ -> "\nReverse limit Ranges"
            Except tags ->
                "\nExcept:"
                |> Str.concat (List.walk tags "" (\ print, inTag ->
                    Str.concat print (printTag inTag) ))
            Only tags ->
                "\nOnly:"
                |> Str.concat (List.walk tags "" (\ print, inTag ->
                    Str.concat print (printTag inTag) ))
            _ ->
                "\n unknown token "
        )

printTokens : List  TokenType -> Str
printTokens = (  \ arrayPrt ->
    List.walk arrayPrt "" (  \ inState , token ->
        Str.concat  inState (printTag token.tag)
        |> Str.concat (printSerie token.serie)
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

getRegexTokens : {tag : RecoverRegexTagsType,parsedResult : ParsingResultType} -> Result (List  TokenType) Str
getRegexTokens = \ result  ->
    when result.tag is
        Character->
            when List.first result.parsedResult.matched is
                Ok  matched  ->
                    Ok [(createToken  ( Character matched )  Once Bool.false )]
                Err  _  -> Err "character  tag problem"
        _ ->
            Err "wrong tag"


regexCreationStage : Str, List TokenPerRegexType, List TokenPerRegexType, List {tag : RecoverRegexTagsType,parsedResult : ParsingResultType} -> Result ( List TokenType )  Str
regexCreationStage  = \ str, patterns, onlyExceptPatterns, currReg ->
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

                            modifLastInChain : List TokenType, TokenType -> List TokenType
                            modifLastInChain = (\ chainLst, token ->
                                when List.last chainLst is
                                    Ok elem ->
                                        when elem.tag is
                                            Sequence  chain ->
                                                when List.last chain is
                                                    Ok lastOnChain ->
                                                        closeLast = (\  lst, seq ->
                                                            Sequence  seqChain = seq
                                                            when List.last seqChain is
                                                                Ok  item  ->
                                                                    when item.tag is
                                                                        Sequence  inChain ->
                                                                            Utils.modifyLastInList
                                                                                lst (createToken (Sequence (closeLast seqChain (Sequence  inChain) ))  Once Bool.false)
                                                                        _ ->
                                                                            List.append lst token
                                                                Err _ ->
                                                                        lst
                                                                        )
                                                        when token.tag is
                                                            CaptureClose ->
                                                                closeLast chainLst (Sequence chain)
                                                               # List.append chainLst token
                                                            _ ->
                                                                Utils.modifyLastInList chainLst (createToken (Sequence ( modifLastInChain chain token))  Once Bool.false)

                                                    Err _ ->
                                                        Utils.modifyLastInList  chainLst  (createToken (Sequence ( modifLastInChain chain token))  Once Bool.false)
                                            _ ->
                                                List.append chainLst token
                                    Err _ ->
                                        List.append chainLst token

                            )

                            updateSerie : TokenType, SerieType -> Result TokenType Str
                            updateSerie = \ token, serie ->
                                when token.serie is
                                    Once ->
                                        Ok { token  & serie : serie }

                                    _ -> Err "Wrong usage of +/*/Repetition/? in pattern"


                            # bit weird  but keep it for now
                            updateSerieIfApplicable : List TokenType, SerieType -> Result ( List TokenType )  Str
                            updateSerieIfApplicable = ( \ outLst, outSerie ->

                                updateSerieIfApplicableInternal : List TokenType, SerieType, Nat, List  TokenType  -> Result ( List TokenType )  Str
                                updateSerieIfApplicableInternal = ( \ inLst, serie, cnt, sequences ->
                                    when List.last inLst is
                                        Ok token ->
                                            when token.tag is
                                                CaptureClose ->
                                                    updatedSeq =
                                                        when sequences is
                                                            [..,lastSeq] ->
                                                                when lastSeq.tag is
                                                                    Sequence chain  ->
                                                                        # with good synchronization it should be ok
                                                                        List.dropLast chain 1
                                                                        |> ( \updatedChain ->
                                                                            Utils.modifyLastInList sequences (createToken (Sequence updatedChain) lastSeq.serie lastSeq.capture ))
                                                                    _ -> sequences
                                                            []  ->  []

                                                    when updateSerieIfApplicableInternal ( List.dropLast inLst  1) serie (cnt + 1) updatedSeq is
                                                        Ok updatedLst ->
                                                            Ok (modifLastInChain updatedLst token)
                                                        Err message -> Err message
                                                Sequence chain ->
                                                    if List.isEmpty sequences then

                                                        when updateSerieIfApplicableInternal chain serie cnt (List.append sequences token) is
                                                            Ok rolledSeqStack->

                                                                Ok (
                                                                    List.dropLast inLst 1
                                                                    |> List.concat  rolledSeqStack)
                                                            Err message -> Err message
                                                    else
                                                        updateSerieIfApplicableInternal chain serie  cnt (List.append sequences token)
                                                _ ->
                                                    if List.isEmpty sequences then
                                                        when updateSerie token serie is
                                                            Ok updatedtoken ->
                                                                Ok (Utils.modifyLastInList inLst updatedtoken)
                                                            Err message -> Err message
                                                    else
                                                        if cnt > 0 then
                                                            List.dropLast sequences (cnt - 1)
                                                            |> List.walkBackwards (Err "internal error during +/*/Repetition/? evaluation ") (  \ createResult , elemToken   ->
                                                                when elemToken.tag is
                                                                    Sequence chain ->
                                                                        when  createResult is
                                                                            Ok updatedSeqLst ->
                                                                                when updatedSeqLst is
                                                                                    [ updatedSeq ] -> Ok [createToken (Sequence (Utils.modifyLastInList chain updatedSeq)) elemToken.serie elemToken.capture ]
                                                                                    _ -> Err "Wrong usage of +/*/Repetition/? in pattern"
                                                                            _-> Ok [createToken (Sequence chain) serie elemToken.capture ]
                                                                    _->
                                                                        Err "Wrong usage of +/*/Repetition/? in pattern" )
                                                        else
                                                            List.walkBackwards sequences (Err "internal error during +/*/Repetition/? evaluation ") (\ createResult, elemToken ->

                                                                when elemToken.tag is
                                                                    Sequence chain ->
                                                                        when  createResult is
                                                                            Ok updatedSeqLst ->
                                                                                when updatedSeqLst is
                                                                                    [ updatedSeq ] -> Ok [createToken (Sequence (Utils.modifyLastInList chain updatedSeq)) elemToken.serie elemToken.capture ]
                                                                                    _ -> Err "Wrong usage of +/*/Repetition/? in pattern"
                                                                            _ ->
                                                                                when (updateSerieIfApplicableInternal chain serie 0 []) is
                                                                                    Ok updatedChain ->
                                                                                        Ok [createToken (Sequence updatedChain) elemToken.serie elemToken.capture ]
                                                                                    Err message -> Err message
                                                                    _->
                                                                        Err "Wrong usage of +/*/Repetition/? in pattern" )
                                        Err _ -> Err  "Wrong usage of +/*/Repetition/? in pattern"
                                    )
                                updateSerieIfApplicableInternal outLst outSerie 0 [] )


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
                                FromTo ->
                                    when (getValue [0] 0 result.parsedResult.captured,
                                        getValue [1] 0 result.parsedResult.captured ) is
                                        ( Ok fromLst, Ok  toLst ) ->
                                            when (fromLst,  toLst) is
                                                ([left], [right] ) ->
                                                    Ok { state &  lst : modifLastInChain state.lst (createToken (LimitRanges [ { left : left, right : right } ]) Once Bool.false )  }
                                                _ -> Err "problem creation range block"
                                        _ -> Err "problem creation range block"

                                Only ->
                                    when (getValue [0] 0 result.parsedResult.captured) is
                                        Ok lst ->
                                            when regexCreationStage (Utils.utfToStr lst) onlyExceptPatterns onlyExceptPatterns [] is
                                                Ok tokensResult ->
                                                    when mergeTokens tokensResult is
                                                        Ok tagLst ->

                                                            Ok { state &  lst : modifLastInChain state.lst (createToken (Only tagLst) Once (doCapture state.capture) ) }
                                                        Err message -> Err message

                                                Err message -> Err message

                                        Err _ -> Err "problem creation only block"

                                Except ->
                                    when (getValue [0] 0 result.parsedResult.captured) is
                                        Ok lst ->
                                            when regexCreationStage (Utils.utfToStr lst) onlyExceptPatterns onlyExceptPatterns [] is
                                                Ok tokensResult ->
                                                    when mergeTokens tokensResult is
                                                        Ok tagLst ->

                                                            Ok { state &  lst : modifLastInChain state.lst (createToken (Except tagLst) Once (doCapture state.capture) ) }
                                                        Err message -> Err message

                                                Err message -> Err message

                                        Err _ -> Err "problem creation except block"

                                BackSlash->
                                    when result.parsedResult.matched is
                                        [backslash, sign ] ->
                                            Ok { state &  lst : modifLastInChain state.lst  (createToken (Character sign) Once (doCapture state.capture)) }
                                        _ -> Err "back slash parser  match problem"

                                Repetition ->
                                    when createRepetitionOutOfTree result.parsedResult.captured is
                                        Ok serie ->
                                            when updateSerieIfApplicable state.lst serie is
                                                Ok newLst ->
                                                    Ok { state &  lst : newLst }
                                                Err message -> Err message
                                        Err message ->
                                            Err message
                                RangeRepetition ->

                                    when createRepetitionOutOfTree result.parsedResult.captured is
                                        Ok serie ->
                                            when updateSerieIfApplicable state.lst serie is
                                                Ok newLst ->
                                                    Ok { state &  lst : newLst }
                                                Err message -> Err message
                                        Err message ->
                                            Err message
                                ZeroOrMore ->
                                    when updateSerieIfApplicable state.lst  ZeroOrMore is
                                        Ok newLst ->
                                            Ok { state &  lst : newLst }
                                        Err message -> Err message
                                Optional ->
                                     when updateSerieIfApplicable state.lst ( NoMorethan 1 ) is
                                        Ok newLst ->
                                            Ok { state &  lst : newLst }
                                        Err message -> Err message
                                AtLeastOne ->


                                    when updateSerieIfApplicable state.lst (AtLeastOne) is
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

createRegexFromData : List  RecoverPatternType, Result ( List  TokenPerRegexType ) Str, Result ( List  TokenPerRegexType ) Str -> Result  ( List  TokenPerRegexType ) Str
createRegexFromData = \ inRegexData, regexBase, onlyExceptBase ->

    when (regexBase, onlyExceptBase) is
        (Ok base, Ok onlyExcept) ->
            List.walkUntil  inRegexData (Ok []) ( \ state, pat  ->
                when state is
                    Ok currentPatterns ->
                        patternResult = regexCreationStage pat.str base onlyExcept []
                        when patternResult is
                            Ok pattern ->
                                Continue (Ok ( List.append currentPatterns { tag : pat.tag, tokens : pattern }))
                            Err message -> Break (Err message)
                    Err message -> Break (Err message)
                    )

        _ -> Err "error in regex patterns creation"


# walkaround for static initialized every time
MagicType : ( Result (List TokenPerRegexType ) Str,  Result (List TokenPerRegexType ) Str )
regexMagic : MagicType
regexMagic = (thirdStageChains, onlyExceptInternalChains)

parseStrMagic : Str, Str, MagicType -> Result ParsingResultType  Str
parseStrMagic = \ str, pattern, magic ->
    when magic is
        (Ok mainRegex, Ok onlyExceptRegex) ->

            tokensFromUserInputResult = regexCreationStage pattern mainRegex onlyExceptRegex []

            when tokensFromUserInputResult is
                Ok tokensFromUserInput ->
                    independentChainlst = splitChainOnSeparators tokensFromUserInput []
                    # for now get longest maybe??
                    Ok (List.walk independentChainlst (createParsingRecord [] Inactive No)  ( \ state, regexParser ->
                        parsResult = checkMatching (Str.toUtf8  str ) regexParser
                        if state.matchFound == Bool.true then
                            if List.len parsResult.matched > List.len state.matched  then
                                parsResult
                            else
                                state
                        else
                            parsResult ))
                Err message ->
                    Err (Str.concat "You screwed up something, or not supported construction, or internal bug \n"  message )

        _ -> Err   "This is internal regex error not your fault\n"



parseStr : Str, Str -> Result ParsingResultType  Str
parseStr = \ str, pattern ->
    when (thirdStageChains, onlyExceptInternalChains) is
        (Ok mainRegex, Ok onlyExceptRegex) ->

            tokensFromUserInputResult = regexCreationStage pattern mainRegex onlyExceptRegex []

            when tokensFromUserInputResult is
                Ok tokensFromUserInput ->
                    independentChainlst = splitChainOnSeparators tokensFromUserInput []
                    # for now get longest maybe??
                    Ok (List.walk independentChainlst (createParsingRecord [] Inactive No)  ( \ state, regexParser ->
                        parsResult = checkMatching (Str.toUtf8  str ) regexParser
                        if state.matchFound == Bool.true then
                            if List.len parsResult.matched > List.len state.matched  then
                                parsResult
                            else
                                state
                        else
                            parsResult ))
                Err message ->
                    Err (Str.concat "You screwed up something, or not supported construction, or internal bug \n"  message )

        _ -> Err   "This is internal regex error not your fault\n"


parseStrMagicFull : Str, Str, MagicType -> Result ( List ParsingResultType)  Str
parseStrMagicFull = \ str, pattern, magic ->
    when magic is
        (Ok mainRegex, Ok onlyExceptRegex) ->

            tokensFromUserInputResult = regexCreationStage pattern mainRegex onlyExceptRegex []

            when tokensFromUserInputResult is
                Ok tokensFromUserInput ->
                    independentChainlst = splitChainOnSeparators tokensFromUserInput []

                    Ok (List.walk independentChainlst []  ( \ state, regexParser ->
                        parsResult = checkMatching (Str.toUtf8  str ) regexParser
                        if parsResult.matchFound == Bool.true then
                            List.append  state parsResult
                        else
                            state ))
                Err message ->
                    Err (Str.concat "You screwed up something, or not supported construction, or internal bug \n"  message )

        _ -> Err   "This is internal regex error not your fault\n"

#parseStrCached : Str, Str, Dict Str (List TokenType)-> Result {parsed : ParsingResultType,cache: Dict Str (List TokenType) }  Str
#parseStrCached = \ str, pattern, cache ->
#    when availableRegex is
#        Ok stage1Pat ->
#            hit = Dict.get cache pattern
#            tokensFromUserInputResult =
#                when hit is
#                    Ok tokens ->
#                        Ok tokens
#                    Err _ ->
#                        regexCreationStage pattern stage1Pat stage1Pat []

#            when tokensFromUserInputResult is
#                Ok tokensFromUserInput ->

#                    independentChainlst = splitChainOnSeparators tokensFromUserInput []
                    # for now get longest maybe??
#                    (List.walk independentChainlst (createParsingRecord [] Inactive No)  ( \ state, regexParser ->
#                        parsResult = checkMatching (Str.toUtf8  str ) regexParser
#                        if state.result == Bool.true then
#                            if List.len parsResult.matched > List.len state.matched  then
#                                parsResult
#                            else
#                                state
#                        else
#                            parsResult ))
#                    |> ( \ parsed ->
#                        when hit is
#                            Ok tokens ->
#                                 Ok { parsed : parsed, cache : cache }
#                            Err _ ->
#                                Ok { parsed : parsed, cache : Dict.insert cache pattern tokensFromUserInput }
 #                           )
 #               Err message ->
 #                   Err (Str.concat "You screwed up something, or not supported construction, or internal bug \n"  message )

  #      Err message ->
  #          Err  (Str.concat "This is internal regex error not your fault\n"  message )





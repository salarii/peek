interface  SearchText
    exposes [evalSearch]
    imports [
        Regex.{MagicType},
        State.{PatternType,ConfigType,SectionType},
        Utils]

# some operations will be slow : ) so it will be up to user to decide which one
# to use in given circumstances

LineSearchResultType : [ Hit (Set PatternType), Miss ]

LineInput : {number : U32, line : Str }

LineAnalyzedType : { content : List  Str, separator : List  Str }

LineProcessedType : { number : U32 , line : LineAnalyzedType, content : Str, status : LineSearchResultType}

mergeLineProcessed : List LineProcessedType, List LineProcessedType, Set PatternType -> List LineProcessedType
mergeLineProcessed = \ leftLst, rightLst, keep ->
    merged =
        List.concat leftLst rightLst
        |> List.sortWith ( \ left, right ->
            if left.number > right.number then
                GT
            else if left.number == right.number then
                EQ
            else  
                LT)
        #  bit ugly but needed at least for now 
        |> List.walk ( 0,[]) (\ state, line -> 
            if line.number != state.0 then 
                (line.number, List.append state.1 line)
            else 
                when List.last state.1 is 
                    Ok last ->
                        (state.0, Utils.modifyLastInList state.1 { last & status : mergeStatus last.status line.status })
                    _ -> state
            )
    merged.1

coloring : List LineProcessedType, ConfigType -> Result (List LineProcessedType ) Str
coloring = \ lines, config ->
    gatherColors = 
        List.walk config.patterns (Set.empty {}) (\state,  pattern -> 
            when pattern is 
                Color _->
                    Set.insert state pattern
                _ -> 
                    state ) 

    colored =
        List.map  lines (\ line->
            Set.walk gatherColors (Ok line) (\ stateResult, pattern ->
                when stateResult is 
                    Ok state ->
                        when searchPattern line.content pattern config is 
                            Ok searchResult -> 
                                if searchResult.status != Miss then
                                    
                                    Ok { state & line : mergeColors [state.line, searchResult.line] }
                                else
                                    Ok state
                            Err message -> Err message
                    Err message -> Err message
                ))
    (List.walkUntil colored (Bool.false, "") ( \ isErrorFlag, result ->
            when  result is 
                Ok _ -> Continue isErrorFlag
                Err message -> Break (Bool.false, message)
            ))
    |> (\ coloringResult -> 
        if coloringResult.0 == Bool.true then
            Err coloringResult.1 
        else 
            Ok (List.map colored (\ lineColRes ->
                when lineColRes is 
                   Ok coloredLine -> coloredLine
                   Err _ -> { number : 0, line : { content : [], separator : [] }, content : "", status : Miss }
                )
            ))  
       
createRawOut : List Str, List LineProcessedType -> Result  Str  Str
createRawOut = \ rawLst, outLst ->
    List.map outLst ( \line -> line.number )
    |> List.walkTry  "" ( \ outStr, idx ->
        when List.get rawLst (Num.toNat (idx-1)) is 
            # when I have done this by accident it crashed 
            # Ok rawLine ->
            #         Str.concat outStr rawLine
            #         |> Str.concat "\n"                    
            Ok rawLine ->
                Ok (
                    Str.concat outStr rawLine
                    |> Str.concat "\n"                    
                )
            Err _ -> Err "problem in processing output" )
            
evalSearch : List Str, ConfigType -> { terminal: Str, raw : Str }
evalSearch = \ content, config ->
    matchAll = Set.contains  config.modifiers LogicAND
    numIdxLen = Utils.strUtfLen( Num.toStr (List.len content) )
    printLine :  LineProcessedType, (Bool, Nat) -> Str 
    printLine = \ processed, printLineNumber -> 
        if printLineNumber.0 == Bool.true then 
            lineNumber = 
                Num.toStr  processed.number 
                |> Utils.fillSpacesUpTo printLineNumber.1         
            ""
            |> Str.concat lineNumber
            |> Str.concat (produceOutput processed.line)
            |> Str.trimEnd
            |> Str.concat "\n\r"
        else
            ""
            |> Str.concat (produceOutput processed.line)
            |> Str.trimEnd
            |> Str.concat "\n\r"

    if List.isEmpty content == Bool.true then
        { terminal: "", raw : "" }
    else 
        # this  should  be  done  in some other place
        lineNembersAdded = List.walk content ([],1) (\state,  line -> 
            # I have done by accident and this crashed compiler
            #  (List.append state.1  {number : state.1, line : line }, state.2 + 1 )
                if List.isEmpty config.limit == Bool.true then
                    (List.append state.0  {number : state.1, line : line }, state.1 + 1 )
                else
                    inSection =
                        List.walkUntil config.limit Bool.false (\ isIn, limit->
                            FromLineToLine left right = limit
                            if (Num.toU32 left) <= state.1 &&
                            (Num.toU32 right) >= state.1 then
                                Break Bool.true
                            else
                                Continue isIn
                        )
                    if inSection == Bool.true then
                        (List.append state.0  {number : state.1, line : line }, state.1 + 1 )        
                    else
                        (state.0, state.1 + 1 )
            )
        
        when config.command is 
            Search -> 
                List.walkTry lineNembersAdded.0 [] (\ register, line->
                    analyseLine line config.patterns register matchAll config
                )
                |> ( \searchResult ->
                    when searchResult is
                        Ok searched ->
                            when coloring searched config is 
                                Ok colored ->
                                    when createRawOut content colored  is 
                                        Ok rawStr -> 
                                            { 
                                                terminal:
                                                    List.walk colored "" (\ out, item ->
                                                        out 
                                                        |>Str.concat (printLine item (Set.contains  config.modifiers NumberLines, numIdxLen + 1))
                                                    ),
                                                raw : rawStr
                                            }
                                        Err message -> { terminal: message, raw : "" }
                                Err message -> { terminal: message, raw : "" }

                        Err message -> { terminal: message, raw : "" }
                        )
            FromPatternToPattern fromPat toPat ->
                patternsProcessedResult =
                    List.walkTry lineNembersAdded.0 [] (\ register, line->
                        analyseLine line config.patterns register matchAll config
                    )
                rangesProcessedResult =
                    List.walkTry lineNembersAdded.0 [] (\ register, line->
                        analyseLine line [fromPat, toPat] register Bool.false config
                    )
                when (patternsProcessedResult, rangesProcessedResult )  is 
                    (Ok patternsProcessed, Ok ranges ) ->
                        mergeLineProcessed patternsProcessed ranges (Set.fromList [fromPat, toPat])
                        |> List.walk (Block,[],[]) (\state, line ->
                            when line.status is 
                                Hit patterns -> 
                                    if Set.contains patterns fromPat == Bool.true then
                                        if state.0 == Allow then
                                            when List.first state.1 is 
                                                Ok head -> 
                                                    (Allow,[line],List.append state.2 [head])
                                                Err _ -> state
                                        else 
                                            (Allow,[line],state.2)
                                    else if Set.contains patterns toPat == Bool.true then
                                        if state.0 == Allow then
                                            (Block,[],List.append state.2 (List.append state.1 line))
                                        else 
                                            (Block,[],List.append state.2 [line])
                                    else
                                        if state.0 == Allow then
                                            (Allow,List.append state.1 line, state.2 )
                                        else 
                                            state
                                Miss -> 
                                    if state.0 == Allow then
                                        (Allow,List.append state.1 line, state.2 )
                                    else 
                                        state
                        )
                        |> ( \ regions -> 
                            List.walkTry regions.2 "" (\ totalOut, region ->
                                when coloring region config is
                                    Ok colored ->
                                        regionOut =
                                            List.walk colored "" (\ out, line ->
                                                out
                                                |> Str.concat (printLine line (Set.contains  config.modifiers NumberLines ,numIdxLen + 1))
                                                )
                                                
                                        Ok ( 
                                            Str.concat totalOut regionOut
                                            |> Str.concat "\n\r-----------------------------\n\r "
                                            )
                                    Err message -> Err message
                            )
                            |> ( \ outStrResult ->
                                when outStrResult is 
                                    Ok outStr -> 
                                        when createRawOut content (List.join regions.2)  is 
                                            Ok rawStr -> 
                                                { 
                                                    terminal: outStr,
                                                    raw : rawStr
                                                }
                                            Err message -> { terminal: message, raw : "" }
                                    Err message -> { terminal: message, raw : "" }
                                 )
                            )
                                
                    (Err message , _ ) ->  { terminal: message, raw : "" }
                    (Ok _, Err message ) -> { terminal: message, raw : "" }
            SearchSection sectionsLst ->        
                patternsProcessed =
                    List.walkTry lineNembersAdded.0 [] (\ register, line->
                        analyseLine line config.patterns register matchAll config
                    )
                gatherPatterns = List.walk sectionsLst [] (\ lst, section -> List.append lst section.pattern )
                hotProcessed =
                    List.walkTry lineNembersAdded.0  [] (\ register, line->
                            analyseLine line gatherPatterns register Bool.false config
                    )
                when (patternsProcessed, hotProcessed )  is 
                    (Ok patterns, Ok hot ) ->
                        merged = mergeLineProcessed patterns hot (Set.fromList gatherPatterns)
                        sections = filterSections merged sectionsLst

                        List.walkTry sections "" (\ totalOut, sec -> 
                                when coloring sec config is
                                    Ok colored ->
                                        sectionOut =
                                            List.walk colored "" (\ out, line ->
                                                out
                                                |> Str.concat (printLine line (Set.contains  config.modifiers NumberLines ,numIdxLen + 1))
                                                )
                                        Ok 
                                            (
                                                Str.concat totalOut sectionOut
                                                |> Str.concat "\n\r-----------------------------\n\r"  
                                            )
                                    Err message -> Err message
                            )
                        |> (( \ outStrResult ->
                                when outStrResult is 
                                    Ok outStr -> 
                                        when createRawOut content (List.join sections)  is 
                                            Ok rawStr -> 
                                                { 
                                                    terminal: outStr,
                                                    raw : rawStr
                                                }
                                            Err message -> { terminal: message, raw : "" }
                                    Err message -> { terminal: message, raw : "" }
                                 )
                            
                            
                        )
                    (Err message , _ ) -> { terminal: message, raw : "" }
                    (Ok _, Err message ) -> { terminal: message, raw : "" }        
            _ -> { terminal: "not supported yet", raw : "" }

decomposeEmpty : 
    {
        allow : Set PatternType, 
        block : Set PatternType,
        andDecomposed : List { pattern: PatternType, decomposed : (Set PatternType, Set PatternType)},
    }
decomposeEmpty = 
    {
        allow : Set.empty {},
        block : Set.empty {},
        andDecomposed : [],
    }

mergeStatus : LineSearchResultType, LineSearchResultType -> LineSearchResultType
mergeStatus = \ leftStatus, rightStatus ->
    when (leftStatus, rightStatus) is 
        (Hit patLstLast, Hit patLstLine) ->
            Hit ( Set.union  patLstLast patLstLine)
        (Hit _, Miss) | (Miss, Miss) ->
            leftStatus
        (Miss, Hit _) ->
            rightStatus              

# ConfigType walkaround here
analyseLine : LineInput, List PatternType, List LineProcessedType, Bool, ConfigType -> Result (List LineProcessedType) Str   
analyseLine = \ lineData, patterns, register, matchAll, config ->
    sortPatterns = 
        List.walk patterns decomposeEmpty (\state,  pattern -> 
            when pattern is 
                Allow pat ->
                    #  I made mistake as below and this caused compiler to hang during build : )
                    # { state &  allow : Set.insert state.allow pattern}  
                    { state &  allow : Set.insert state.allow pattern }
                Blacklist pat -> 
                    { state &  block : Set.insert state.block pattern }
                _ -> 
                    state
                LogicalAnd  patternLst ->
                    List.walk patternLst (Set.empty {},Set.empty {}) ( \ inState, andPattern -> 
                        when  andPattern is 
                            Allow pat ->  
                                (Set.insert inState.0 (Allow pat), inState.1)
                            Blacklist pat -> 
                                (inState.0, Set.insert inState.1 (Blacklist pat))
                        )
                    |> (\ andSet -> 
                        { state &
                            andDecomposed : List.append state.andDecomposed {pattern: LogicalAnd  patternLst, decomposed : andSet},
                        } )
                _-> state
                )
    miss = { number : lineData.number, line : { content : [lineData.line], separator : [] }, content: lineData.line,status : Miss }
    allowStageResult = 
        if (Set.isEmpty sortPatterns.allow == Bool.false ||
            List.isEmpty sortPatterns.andDecomposed == Bool.false) then
            List.walkUntil sortPatterns.andDecomposed (Ok miss ) (\ stateResult, andSet ->
                when stateResult is 
                    Ok state ->
                        (Set.walkUntil andSet.decomposed.0 (Ok miss) ( \ searchStateResult, pattern -> 
                            when searchStateResult is 
                                Ok searchState ->
                                    when searchPattern lineData.line pattern config is 
                                        Ok searchResult -> 
                                            if searchResult.status != Miss then
                                                Continue ( Ok { searchState & line : mergeColors [searchState.line, searchResult.line], status : mergeStatus searchState.status searchResult.status})
                                            else
                                                Break ( Ok  miss )
                                        Err message -> Break (Err message)
                                Err message -> Break (Err message)
                            )
                        )
                        |> ( \ allowResult -> 
                            when allowResult is
                                Ok allow ->
                                    if allow.status != Miss then
                                        #
                                        (Set.walkUntil andSet.decomposed.1  (Ok miss)  ( \ searchStateResult, pattern -> 
                                                when searchStateResult is 
                                                    Ok searchState ->
                                                        when searchPattern lineData.line pattern config is 
                                                            Ok searchResult -> 
                                                                if searchResult.status == Miss then
                                                                    Continue ( Ok searchState )
                                                                else
                                                                    Break ( Ok {searchState & status : searchResult.status} )
                                                            Err message -> Break (Err message)                
                                                    Err message -> Break (Err message)
                                                ))
                                            |>  ( \ blockResult -> 
                                                when blockResult is 
                                                    Ok block -> 
                                                        if block.status == Miss then
                                                            Break (Ok {state &  status : Hit (Set.empty {} |> Set.insert andSet.pattern)})           
                                                        else 
                                                            Continue  (Ok state)     
                                                    Err message -> Break (Err message)
                                                    )
                                    else 
                                        Continue  (Ok state)
                                Err message -> Break (Err message) 
                                )
                    Err message -> Break (Err message)
                    )
                |> (\ andResult ->
                    when andResult is 
                        Ok and ->
                            if and.status != Miss then
                                Ok and
                            else
                                Set.walkUntil sortPatterns.allow (Ok miss) (\ stateResult, pattern ->
                                    when stateResult is 
                                        Ok state ->
                                            when searchPattern lineData.line pattern config is 
                                                Ok searchResult -> 
                                                    if searchResult.status != Miss then
                                                        Continue ( Ok { state & line : mergeColors [state.line, searchResult.line], status : mergeStatus state.status searchResult.status})
                                                    else
                                                        Continue ( Ok state )
                                                Err message -> Break (Err message)
                                        Err message -> Break (Err message)
                                    )
                        Err message -> Err message
                    )
        else
            Ok miss
    when  allowStageResult is 
        Ok allowStage ->
            if (Set.isEmpty sortPatterns.allow == Bool.false ||
               List.isEmpty sortPatterns.andDecomposed == Bool.false ) &&
               allowStage.status == Miss then
                Ok register
            else
                if Set.isEmpty sortPatterns.block == Bool.false then
                    Set.walkUntil sortPatterns.block (Ok Miss) (\ stateResult, pattern ->
                            when  stateResult is
                                Ok state ->
                                    when searchPattern lineData.line pattern config is 
                                        Ok searchResult -> 
                                            if searchResult.status != Miss then
                                                Break (Ok searchResult.status)
                                            else
                                                Continue stateResult
                                        Err message -> Break (Err message)
                                Err message -> Break (Err message)
                        )
                    |> (\ searchResult ->
                        when searchResult is 
                            Ok searched  ->
                                if searched == Miss then
                                    Ok (List.append register allowStage)
                                else 
                                    Ok register
                            Err message -> Err message)
                else
                    Ok (List.append  register allowStage)
        Err message -> Err message 

searchPattern : Str, PatternType, ConfigType -> Result { line : LineAnalyzedType, status : LineSearchResultType }  Str
searchPattern = \ line, pattern, config ->
    when pattern is 
        Allow (Regex pat) | Color (Regex pat) | Blacklist (Regex pat) -> 
                    when regexWordMatch line pat { content : [], separator : [] } config.regexMagic is 
                        Ok searched -> 
                            if List.isEmpty searched.separator == Bool.false then
                                if List.len searched.separator + 1 == List.len searched.content then
                                    Ok  { line : searched, status : Hit (Set.empty {} |> Set.insert pattern) }
                                else
                                    Err "problem during search \(pat)"
                            else
                               Ok  { line : searched, status : Miss }
                        _-> Err ("unknown problem during search")
                    
        Allow (Plain pat) | Color (Plain pat) | Blacklist (Plain pat) -> 
            searched = plainWordMatch line pat 
            if List.isEmpty searched.separator == Bool.false then
                Ok { line : searched, status : Hit (Set.empty {} |> Set.insert pattern)}
            else
                Ok { line : searched, status : Miss }
        _ -> Err ("unknown problem during search")


mergeColors : List LineAnalyzedType-> LineAnalyzedType
mergeColors = \ lst ->
    ColorType : { char : U8,color : [ Color, Blank ]  }    

    List.walk lst [] ( \ state, line ->    
        when line.content is 
            [ word ] -> 
                # this should never be hit in proper design but just to make it right
                List.append state (List.map (Str.toUtf8 word) ( \ char -> { char : char,color : Blank} ))
            [.. as head, tail] ->
                    colorDefinedLst =
                        List.map2  head  line.separator ( \ left, right->
                            List.map (Str.toUtf8 left) ( \ char -> { char : char,color : Blank} )
                            |> List.concat ( List.map (Str.toUtf8 right) ( \ char -> { char : char,color : Color} ) )  )
                        |> List.join
                        |> List.concat (List.map (Str.toUtf8 tail) ( \ char -> { char : char,color : Blank} ))
                    List.append state colorDefinedLst
            [] -> List.append state  []
            )
    |> (\ colorDefLst ->
            when colorDefLst is
                [head, .. as tail] ->
                    List.walk  tail head ( \ state, new -> 
                        List.map2 state new ( \ left, right ->
                            if left.color == Color then
                                left 
                            else if right.color == Color then
                                right
                            else
                                left
                        ))
                    |> List.walk { last : Blank, joined : {content : [""], separator : [] } } ( \ state,colorChar -> 
                        appendArray : List Str, U8 -> List Str
                        appendArray = \ array, char ->
                            List.append array (Utils.utfToStr [char] )

                        joinArray : List Str, U8 -> List Str
                        joinArray = \ array, char ->
                            when array is 
                                [.., lastElem] ->
                                    Utils.modifyLastInList array (Str.concat lastElem (Utils.utfToStr [char] ) ) 
                                [] ->
                                    List.append array (Utils.utfToStr [char] )
                         

                        if colorChar.color == Blank then
                            if state.last == Blank then 
                                {state & joined : {content : joinArray state.joined.content  colorChar.char, separator : state.joined.separator  } }
                            else 
                                {last : Blank, joined : {content : appendArray state.joined.content  colorChar.char, separator : state.joined.separator  }}
                        else 
                            if state.last == Blank then 
                                {last : Color, joined : {content : state.joined.content, separator : appendArray state.joined.separator  colorChar.char }}
                            else 
                                {state & joined : {content : state.joined.content, separator : joinArray state.joined.separator  colorChar.char }}
                    ) 
                    |> ( \ mergedData -> 
                        joined = mergedData.joined 
                        if List.len joined.content == List.len joined.separator then 
                            {joined & content : List.append  joined.content "" }
                        else
                            joined
                        )
                [array] -> 
                    {
                        content : [Utils.utfToStr (List.map array ( \ charColor -> charColor.char ))],
                        separator : [],
                    }
                [] ->  {content : [], separator : [] } )

produceOutput : LineAnalyzedType -> Str
produceOutput = \ line ->
    when line.content is 
        [ word ] -> word
        [head,.. as tail ] ->
            head
            |> Str.concat (
                List.map2 line.separator tail (\ left, right -> 
                    Str.concat  ( Utils.withColor left  Red) right
                )
                |> List.walk "" (\ phrase, str ->
                    Str.concat phrase str
                    )
            )
        _ -> ""

plainWordMatch : Str, Str -> LineAnalyzedType
plainWordMatch = \ word, pattern ->
    when (Str.split  word  pattern ) is
        [one] ->
            { content : [word], separator : [] }  
        lst -> 
            { content : lst, separator : List.repeat  pattern ((List.len lst) - 1) }

regexWordMatch : Str, Str, LineAnalyzedType, MagicType -> Result LineAnalyzedType  Str
regexWordMatch = \ word, pattern, register, magic ->
    if  Str.isEmpty word then 
        if List.len register.content == List.len register.content  then
            Ok { register & content : List.append register.content "" } 
        else  
            Ok register 
    else
        when Regex.parseStrMagic word pattern magic is 
            Ok parsed ->
                if parsed.matchFound == Bool.true then 
                        current =
                        {
                            content : List.append register.content (Utils.utfToStr parsed.missed),
                            separator : List.append register.separator (Utils.utfToStr parsed.matched)
                        }
                        regexWordMatch (Utils.utfToStr parsed.left) pattern current magic
                else
                    Ok ( {content : List.append register.content word, separator : register.separator })
            Err message -> Err message 

# in this function I want to experiment with tuples as state in walk (will I be able to maintain this in long run ??)
filterSections : List LineProcessedType, List SectionType -> List (List LineProcessedType)
filterSections = \ lines, sections  -> 
    if List.len  sections == 0 then
        []
    else
        longest = List.walk sections 0  
            ( \ len , pat  -> 
                if pat.before > len then
                    pat.before
                else
                    len )

        getSizeLast : List (List a) -> Nat
        getSizeLast = ( \ listOfLst  ->
            when List.last listOfLst is 
                Ok lst -> List.len lst
                Err _ -> 0 )
    
        evalIfHit : LineProcessedType -> (Bool, Nat, Nat)
        evalIfHit = \ line -> 
            getLonger : Nat, Nat -> Nat
            getLonger = \ left, right ->
                if  left >  right then
                    left
                else
                    right 
            List.walkUntil sections  (Bool.false, 0, 0) (\ state, section->
                when  line.status is 
                    Hit  patterns -> 
                        if Set.contains patterns section.pattern == Bool.true then 
                             Continue (Bool.true , getLonger state.1 section.before, getLonger state.2 section.after ) 
                        else 
                            Continue state
                    Miss -> Continue state 
                )
    
        nextLineParse : {cnt : Nat,buffer : List LineProcessedType, outBuffers: List (List LineProcessedType)}, LineProcessedType -> {cnt : Nat,buffer : List LineProcessedType, outBuffers: List (List LineProcessedType)}
        nextLineParse = (\  state, line ->
            parsingResult = evalIfHit line 
            if parsingResult.0 == Bool.true  then  

                state
                |> ( \ stateIn ->

                    if parsingResult.1 > (getSizeLast stateIn.outBuffers) then
                        { stateIn &
                            outBuffers : 
                                List.dropLast  stateIn.outBuffers 1
                                |> List.append  ( List.sublist stateIn.buffer { start: (longest - parsingResult.1 ), len: parsingResult.1 + 1 } )
                        }
                    else
                        lastExtended = 
                            when  List.last stateIn.outBuffers is
                                    Ok lst -> 
                                        Utils.modifyLastInList stateIn.outBuffers  (List.append lst line)
                                    Err _ ->  List.append stateIn.outBuffers [line] 
                        { stateIn & outBuffers : lastExtended }
                        
                )
                |> ( \ modState ->     
                        if parsingResult.2 >= modState.cnt  then
                            { modState & cnt : parsingResult.2 }
                        else
                            { modState & cnt : modState.cnt - 1 } )
            else 
                updatedBuffer =
                    when  List.last state.outBuffers is
                                Ok lst -> 
                                    Utils.modifyLastInList state.outBuffers  (List.append lst line)
                                Err _ ->  List.append state.outBuffers [line]
                if state.cnt > 0 then
                    { state & cnt : state.cnt - 1, outBuffers : updatedBuffer }
                else
                    when  List.last state.outBuffers is
                        Ok lst -> 
                            if List.len lst > 0 then
                                { state &  outBuffers : List.append state.outBuffers  [] }
                            else
                                state
                        Err _ -> state )
                    
        analysysDone = 
            List.walk lines {cnt : 0,buffer : [], outBuffers : []}
                (\ state , line ->
                    state 
                    |> ( \ inState ->
                        if List.len inState.buffer <= longest then
                            { inState & buffer : List.append inState.buffer line}
                        else
                            { inState &
                                 buffer : 
                                    List.dropFirst inState.buffer 1
                                    |> List.append  line })
                    |> nextLineParse line 
                )
        List.dropIf analysysDone.outBuffers (\lst -> List.isEmpty lst)
  
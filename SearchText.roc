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

mergeLineProcessed : List LineProcessedType, List LineProcessedType -> List LineProcessedType
mergeLineProcessed = \ leftLst, rightLst ->
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
        # this  should  be  done  in some other place??
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
                    analyseLine line config.patterns register config
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
            FromPatternToPattern regLst ->
                jointPat = List.walk regLst [] ( \outLst, patDeq ->
                    outLst
                    |> List.append patDeq.0
                    |> List.append patDeq.1
                    )
                patternsProcessedResult =
                    List.walkTry lineNembersAdded.0 [] (\ register, line->
                        analyseLine line config.patterns register config
                    )
                rangesProcessedResult =
                    List.walkTry lineNembersAdded.0 [] (\ register, line ->
                        analyseLine line jointPat register config
                    )
                when (patternsProcessedResult, rangesProcessedResult )  is
                    (Ok patternsProcessed, Ok ranges ) ->
                        mergedLines = mergeLineProcessed patternsProcessed ranges
                        List.map regLst (\ pair  ->
                            List.walk mergedLines (Block,[],[]) (\state, line ->
                                when line.status is
                                    Hit patterns ->
                                        if Set.contains patterns pair.0 == Bool.true then
                                            if state.0 == Allow then
                                                when List.first state.1 is
                                                    Ok head ->
                                                        (Allow,[line],List.concat state.2 [head])
                                                    Err _ -> state
                                            else
                                                (Allow,[line],state.2)
                                        else if Set.contains patterns pair.1 == Bool.true then
                                            if state.0 == Allow then
                                                (Block,[],List.concat state.2 (List.append state.1 line))
                                            else
                                                (Block,[],List.concat state.2 [line])
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
                        ))
                        |> ( \ searchResult ->
                            when List.last searchResult is
                                Ok searchLst ->
                                    Ok ((List.walk (List.dropLast searchResult 1) searchLst.2 ( \state, patLst ->
                                            mergeLineProcessed state patLst.2
                                    ))
                                    |> List.walk  (Num.toU32 -2,[]) ( \ regions, line ->
                                            if regions.0 + 1 == line.number then
                                                when regions.1 is
                                                    [] ->
                                                        ( line.number, List.append regions.1 [line] )
                                                    [..,last] ->
                                                        ( line.number, Utils.modifyLastInList regions.1 (List.append last line) )
                                            else
                                                ( line.number, List.append regions.1 [line] )
                                        ))
                                Err _ -> Err "unexpected error"
                        )
                        |> ( \ regionsResult ->
                            when regionsResult is
                                Ok regions ->
                                    (List.walkTry regions.1 "" (\ totalOut, region ->
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
                                            Err message -> Err message ))
                                    |> ( \ outStrResult ->
                                        when outStrResult is
                                            Ok outStr ->
                                                when createRawOut content (List.join regions.1)  is
                                                    Ok rawStr ->
                                                        {
                                                            terminal: outStr,
                                                            raw : rawStr
                                                        }
                                                    Err message -> { terminal: message, raw : "" }
                                            Err message -> { terminal: message, raw : "" }
                                        )
                                Err message -> { terminal: message, raw : "" })

                    (Err message , _ ) ->  { terminal: message, raw : "" }
                    (Ok _, Err message ) -> { terminal: message, raw : "" }
            SearchSection sectionsLst ->
                patternsProcessed =
                    List.walkTry lineNembersAdded.0 [] (\ register, line->
                        analyseLine line config.patterns register config
                    )
                gatherPatterns = List.walk sectionsLst [] (\ lst, section -> List.append lst section.pattern )
                hotProcessed =
                    List.walkTry lineNembersAdded.0  [] (\ register, line->
                            analyseLine line gatherPatterns register config
                    )
                when (patternsProcessed, hotProcessed )  is
                    (Ok patterns, Ok hot ) ->
                        merged = mergeLineProcessed patterns hot
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

DecomposeType :
    {
        andDecomposedAllow : List { pattern: PatternType, decomposed : (Set PatternType, Set PatternType)},
        andDecomposedBlock : List { pattern: PatternType, decomposed : (Set PatternType, Set PatternType)},
    }
decomposeEmpty : DecomposeType
decomposeEmpty =
    {
        andDecomposedAllow : [],
        andDecomposedBlock : [],
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


sortPatterns : List PatternType -> DecomposeType
sortPatterns = \ patterns ->
    sortANDTail : List [Allow [ Plain Str, Regex Str], Blacklist [Plain Str, Regex Str]] -> ( Set PatternType, Set PatternType )
    sortANDTail = \ tailPatterns ->
        List.walk tailPatterns (Set.empty {},Set.empty {}) ( \ inState, andPattern ->
            when  andPattern is
                Allow (Plain pat) ->
                    (Set.insert inState.0 (Allow (Plain pat)), inState.1)
                Allow (Regex pat) ->
                    (Set.insert inState.0 (Allow (Regex pat)), inState.1)
                Blacklist (Plain pat) ->
                    (inState.0, Set.insert inState.1 (Blacklist (Plain pat)))
                Blacklist (Regex pat) ->
                    (inState.0, Set.insert inState.1 (Blacklist (Regex pat)))
                _ -> inState
            )

    List.walk patterns decomposeEmpty (\state,  pattern ->
            when pattern is
                Allow (LogicalAnd  patternLst) ->
                    sortANDTail patternLst
                    |> (\ andSet ->
                        { state &
                            andDecomposedAllow : List.append state.andDecomposedAllow {pattern: pattern, decomposed : andSet},
                        } )

                Blacklist (LogicalAnd  patternLst) ->
                    sortANDTail patternLst
                    |> (\ andSet ->
                        { state &
                            andDecomposedBlock : List.append state.andDecomposedBlock {pattern: pattern, decomposed : andSet},
                        } )
                Allow _ ->
                    { state &
                        andDecomposedAllow : List.append state.andDecomposedAllow {pattern: pattern, decomposed : (Set.empty {} |> Set.insert pattern,Set.empty {})}
                    }
                Blacklist _ ->
                    { state &
                        andDecomposedBlock : List.append state.andDecomposedBlock {pattern: pattern, decomposed : (Set.empty {} |> Set.insert pattern,Set.empty {})}
                    }
                _-> state
                )

analyseLine : LineInput, List PatternType, List LineProcessedType, ConfigType -> Result (List LineProcessedType) Str
analyseLine = \ lineData, patterns, register, config ->

    miss = { number : lineData.number, line : { content : [lineData.line], separator : [] }, content: lineData.line,status : Miss }

    blackListSearch : List { pattern: PatternType, decomposed : (Set PatternType, Set PatternType)} -> Result LineProcessedType Str
    blackListSearch = \ decompBlock ->
        List.walkUntil decompBlock (Ok miss ) (\ stateResult, andSet ->
            when stateResult is
                Ok _state ->
                    (Set.walkUntil andSet.decomposed.0 (Ok miss) ( \ searchStateResult, pattern ->
                        when searchStateResult is
                            Ok searchState ->
                                when searchPattern lineData.line pattern config is
                                    Ok searchResult ->
                                        if searchResult.status != Miss then
                                            Break ( Ok { searchState & status : searchResult.status})
                                        else
                                            Continue ( Ok  miss )
                                    Err message -> Break (Err message)
                            Err message -> Break (Err message)
                        ))
                        |> ( \ allowResult ->
                            when allowResult is
                                Ok allow ->
                                    if allow.status == Miss && Set.isEmpty andSet.decomposed.1 == Bool.false then
                                        (Set.walkUntil andSet.decomposed.1 (Ok {miss & status : Hit (Set.empty {} |> Set.insert andSet.pattern)}) (\ searchStateResult, pattern ->
                                            when searchStateResult is
                                                Ok searchState ->
                                                    when searchPattern lineData.line pattern config is
                                                        Ok searchResult ->
                                                            if searchResult.status == Miss then
                                                                Break ( Ok searchState )
                                                            else
                                                                Continue ( Ok miss )
                                                        Err message -> Break (Err message)
                                                Err message -> Break (Err message)
                                        ))
                                    else
                                        Ok allow
                                Err message -> Err message
                                )
                    |> ( \ blockProcessed ->
                        when blockProcessed is
                            Ok processed ->
                                if processed.status == Miss then
                                    Continue ( Ok processed )
                                else
                                    Break ( Ok processed )
                            Err message -> Break (Err message)
                    )
                Err message -> Break (Err message))

    sortedPatterns = sortPatterns patterns
    if List.isEmpty sortedPatterns.andDecomposedAllow == Bool.true then
        when blackListSearch sortedPatterns.andDecomposedBlock is
            Ok blockProcess ->
                if  blockProcess.status == Miss then
                    Ok (List.append register miss)
                else
                    Ok register
            Err message -> Err message
    else
        allowStageResult =
            List.walkUntil sortedPatterns.andDecomposedAllow (Ok miss ) (\ stateResult, andSet ->
                when stateResult is
                    Ok state ->
                        (Set.walkUntil andSet.decomposed.0 (Ok state) ( \ searchStateResult, pattern ->
                            when searchStateResult is
                                Ok searchState ->
                                    when searchPattern lineData.line pattern config is
                                        Ok searchResult ->
                                            if searchResult.status != Miss then
                                                Continue ( Ok { searchState & line : mergeColors [searchState.line, searchResult.line], status : searchResult.status})
                                            else
                                                Break ( Ok  miss )
                                        Err message -> Break (Err message)
                                Err message -> Break (Err message)
                            ))
                        |> ( \ allowResult ->
                            when allowResult is
                                Ok allow ->
                                    if allow.status != Miss || (allow.status == Miss && Set.isEmpty andSet.decomposed.0 == Bool.true) then
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
                                                            hitLst =
                                                                when state.status is
                                                                    Hit patternsLst ->
                                                                        patternsLst
                                                                    Miss  ->  Set.empty {}

                                                            Continue (Ok {allow &  status : Hit (hitLst |> Set.insert andSet.pattern)})
                                                        else
                                                            Continue  (Ok state)
                                                    Err message -> Break (Err message)
                                            )
                                    else
                                        Continue  (Ok state)
                                Err message -> Break (Err message)
                                )
                    Err message -> Break (Err message))
        when  allowStageResult is
            Ok allowStage ->
                if allowStage.status == Miss then
                    Ok register
                else
                    when blackListSearch sortedPatterns.andDecomposedBlock is
                        Ok blockProcess ->
                            if  blockProcess.status == Miss then
                                Ok (List.append register allowStage)
                            else
                                Ok register
                        Err message -> Err message
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
        [_one] ->
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

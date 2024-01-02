interface  SearchText
    exposes [evalSearch]
    imports [
        Regex,
        State.{PatternType,ConfigType},
        Utils]

# some operations will be slow : ) so it will be up to user to decide which one
# to use in given circumstances

LineSearchType : [ Hit, Miss ]

LineInput : {number : U32, line : Str }

LineAnalyzedType : { content : List  Str, separator : List  Str }

LineProcessedType : { number : U32 , line : LineAnalyzedType, status : LineSearchType, color : Set PatternType }

evalSearch : List Str, ConfigType -> Str
evalSearch = \ content, config ->
    if List.isEmpty content == Bool.true then
        ""
    else 
        # this  should  be  done  in some other place
        lineNembersAdded = List.walk content ([],1) (\state,  line -> 
            # I have done by accident and this crashed compiler
            #  (List.append state.1  {number : state.1, line : line }, state.2 + 1 )
            (List.append state.0  {number : state.1, line : line }, state.1 + 1 )
            )
        
        when config.command is 
            Search -> 
                List.walkUntil lineNembersAdded.0 (Ok []) (\ result, line->
                    when result is 
                        Ok resultRegister -> 
                            Continue (analyseLine line config.patterns resultRegister)
                        Err message -> Break (Err message)
                )
                |> ( \searchResult ->
                    when searchResult is
                        Ok searched ->
                            numIdxLen = Utils.strUtfLen( Num.toStr (List.len content) )
                            List.walk searched "" (\ out, item -> 
                            
                                if Set.contains  config.modifiers NumberLines == Bool.true then 
                                    lineNumber = 
                                        Num.toStr  item.number 
                                        |> Utils.fillSpacesUpTo (numIdxLen + 1)
                                    out
                                    |> Str.concat lineNumber
                                    |> Str.concat (produceOutput item.line)
                                    |> Str.concat "\n\r"
                                else
                                    out
                                    |> Str.concat (produceOutput item.line)
                                    |> Str.concat "\n\r"
                            )

                        Err message -> message 
                        )
            _ -> "not supported yet"

analyseLine : LineInput, List PatternType, List LineProcessedType -> Result (List LineProcessedType) Str   
analyseLine = \ lineData, patterns, register ->
    sortPatterns = 
        List.walk patterns {allow : Set.empty {}, block : Set.empty {}, color : Set.empty {} } (\state,  pattern -> 
            when pattern is 
                Regex type ->
                    when type is
                        Allow pat ->
                            { state &  allow : Set.insert state.allow pattern, color : Set.insert state.color (Regex (Color pat ) )  }
                        Blacklist pat -> 
                            { state &  block : Set.insert state.block pattern }
                        Color pat -> 
                            { state &  color : Set.insert state.color pattern }

                Allow pat ->
                    #  I made mistake as below and this caused compiler to hang during build : )
                    # { state &  allow : Set.insert state.allow pattern, color : Set.insert state.color (Color pattern) }  
                    { state &  allow : Set.insert state.allow pattern, color : Set.insert state.color (Color pat) }
                Blacklist pat -> 
                    { state &  block : Set.insert state.block pattern }
                Color pat -> 
                    { state &  color : Set.insert state.color pattern }
                _-> state
                )
    miss = { number : lineData.number, line : { content : [lineData.line], separator : [] }, status : Miss, color : sortPatterns.color }
    allowStageResult = 
        if Set.isEmpty sortPatterns.allow == Bool.false then
            Set.walkUntil sortPatterns.allow (Ok miss) (\ stateResult, pattern ->
                when stateResult is 
                    Ok state ->
                        when pattern is 
                            Regex inside ->
                                when inside is 
                                    Allow pat -> 
                                        when regexWordMatch lineData.line pat { content : [], separator : [] } is 
                                            Ok searched -> 
                                                if List.isEmpty searched.separator == Bool.false then
                                                    if List.len searched.separator + 1 == List.len searched.content then
                                                        Break ( Ok { state & line : searched , status : Hit, color : Set.remove state.color (Color pat)})
                                                    else
                                                        Break (Err "problem during search \(pat)")  
                                                else
                                                    Continue ( Ok { state &  color :  Set.remove state.color (Color pat) } )
                                            Err _ -> Break (Err "problem during search \(pat)")
                                    _-> Break (Err ("unknown problem during search"))
                    
                            Allow pat -> 
                                searched = plainWordMatch lineData.line pat 
                                if List.isEmpty searched.separator == Bool.true then
                                    Continue ( Ok { state &  color :  Set.remove state.color (Color pat) } )
                                else
                                    Break ( Ok { state & line : searched, status : Hit, color : Set.remove state.color (Color pat)})
                            _ -> Break (Err ("unknown problem during search"))
                    Err message -> Break (Err message)
                )
        else
            Ok miss

    when  allowStageResult is 
        Ok allowStage ->
            if Set.isEmpty sortPatterns.allow == Bool.false && 
                allowStage.status == Miss then
                Ok register
            else
                if Set.isEmpty sortPatterns.block == Bool.false then
                    searchResult =
                        Set.walkUntil sortPatterns.block (Ok  { content : [], separator : [] }) (\ state, pattern ->
                            when  state is
                                Ok _ -> 
                                    when pattern is 
                                        Regex type ->
                                            when type is
                                                Blacklist pat -> 
                                                    searchedResult = regexWordMatch lineData.line pat { content : [], separator : [] }
                                                    when searchedResult is 
                                                        Ok searched -> 
                                                            if List.isEmpty searched.separator == Bool.true then        
                                                                Continue state
                                                            else 
                                                                Break (Ok searched)
                                                        Err message ->  Break (Err message)
                                                _ -> Break (Err ("unknown problem during search"))
                                        Blacklist pat -> 
                                            searched = plainWordMatch lineData.line pat
                                            if List.isEmpty searched.separator == Bool.true then        
                                                Continue state
                                            else 
                                                Break (Ok searched)
                                        _ ->  Break (Err ("unknown problem during search"))
                                Err message ->  Break (Err message)
                        )
                    when searchResult is 
                        Ok searched  ->
                            if List.isEmpty searched.separator == Bool.true then
                                Ok (List.append register allowStage)
                            else 
                                Ok register
                        Err message -> Err message
                else
                    Ok (List.append  register allowStage)
        Err message -> Err message 


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
            dbg  colorDefLst
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

regexWordMatch : Str, Str, LineAnalyzedType -> Result LineAnalyzedType  Str
regexWordMatch = \ word, pattern, register ->
    if  Str.isEmpty word then 
        if List.len register.content == List.len register.content  then
            Ok { register & content : List.append register.content "" } 
        else  
            Ok register 
    else 
        when Regex.parseStr word pattern is 
            Ok parsed ->
                if parsed.matchFound == Bool.true then 
                        current =
                        {
                            content : List.append register.content (Utils.utfToStr parsed.missed),
                            separator : List.append register.content (Utils.utfToStr parsed.matched)
                        }
                        regexWordMatch word (Utils.utfToStr parsed.left) current 
                else
                    Ok ( {content : List.append register.content word, separator : register.separator })
            Err message -> Err message 

# wordMatches = \ word, patterns ->
#     List.walk patterns Bool.false ( \flag, pat -> 
#         if flag == Bool.true then
#             Bool.true
#         else
#             when (Str.splitFirst  word  pat ) is
#             Ok _ -> Bool.true
#             Err  _ ->  Bool.false)


# filterInternal = \ str, matches, inFlag  ->
#     if List.isEmpty matches == Bool.true  then
#         str
#     else
#         Str.split str "\n"
#         |> List.walk "" ( \ state , elem ->  
#             checkResult = wordMatches elem  matches
#             if  (checkResult == inFlag) && (Str.isEmpty (Str.trim elem ) == Bool.false) then 
#                 Str.concat  state elem
#                 |>  Str.concat  "\n"
#             else 
#                 state
#             )


# wordMatchesRegion = \ word, patterns ->
#     List.walk patterns { matchResult : Bool.false, pattern :{ pat : "", lines : 0 }  } ( \state, pattern -> 
#         when (Str.splitFirst  word  pattern.pat ) is
#                 Ok _ ->
#                     if (state.matchResult == Bool.true) && (state.pattern.lines >= pattern.lines ) then
#                         state
#                     else                        
#                         { state &  matchResult : Bool.true, pattern : pattern }
#                 Err  _ ->  state  )

# filterRegion = \ inLog, regions  -> 
#     if List.len  regions == 0 then
#         inLog
#     else 
#         longest = List.walk regions 0  
#             ( \ state , pat  -> 
#                 if pat.lines > state then
#                     pat.lines
#                 else
#                     state )
       
#         refreashedRegions = regions
        
#         getSizeLast = ( \ listOfLst  ->
#             when List.last listOfLst is 
#                 Ok lst -> List.len lst
#                 Err _ -> 0 )
    
#         nextLineParse = (\  state, line ->
#             parsingResult = wordMatchesRegion line  refreashedRegions

#             if parsingResult.matchResult == Bool.true  then  

#                 state
#                 |> ( \ stateIn ->

#                     if parsingResult.pattern.lines > (getSizeLast stateIn.bufferMatched) then
#                         { stateIn &
#                             bufferMatched : 
#                                 List.dropLast  stateIn.bufferMatched 1
#                                 |> List.append  ( List.sublist stateIn.buffer { start: (longest - parsingResult.pattern.lines), len: parsingResult.pattern.lines } ) }
#                     else
#                         { stateIn &bufferMatched : 
#                             (
#                                 when  List.last stateIn.bufferMatched is
#                                     Ok lst -> 
#                                         (List.replace stateIn.bufferMatched (List.len stateIn.bufferMatched - 1)  (List.append lst line)).list
#                                     Err _ ->  List.append stateIn.bufferMatched [line] ) })
#                 |> ( \ modState ->     
#                         if parsingResult.pattern.lines > modState.cnt  then
#                             {modState & cnt : parsingResult.pattern.lines }
#                         else
#                             {modState & cnt : modState.cnt - 1  })
#             else 
#                 if state.cnt > 0 then
#                     {state &
#                         cnt : state.cnt - 1,
#                         bufferMatched : 
#                             (
#                                 when  List.last state.bufferMatched is
#                                     Ok lst -> 
#                                         (List.replace state.bufferMatched (List.len state.bufferMatched - 1)  (List.append lst line)).list
#                                     Err _ ->  List.append state.bufferMatched [line] ) }
#                 else
#                     when  List.last state.bufferMatched is
#                         Ok lst -> 
#                             if List.len lst > 0 then
#                             {state &
#                                 bufferMatched :  List.append state.bufferMatched  [] }
#                             else
#                                 state
#                         Err _ -> state )
                    
#         analysysDone = 
#             Str.split inLog "\n"
#             |> List.walk { cnt : 0, buffer : [], bufferMatched : []}
#                 (\ state , line ->
                    
#                     state 
#                     |> ( \ inState ->
#                         if List.len inState.buffer < longest then
#                             { inState & buffer :  List.append inState.buffer line}
#                         else
#                             { inState & buffer : ( 
#                                 List.dropFirst inState.buffer 1
#                                 |> List.append  line  )} )
#                     |> nextLineParse line 
#                 )
#         List.walk analysysDone.bufferMatched ""  
#             ( \ state, lst ->  
#                 if List.isEmpty lst  == Bool.true then
#                     state
#                 else              
#                     List.walk lst state ( \inState , line ->
#                         Str.concat  inState  "\n"
#                         |> Str.concat line ) 
#                     |>Str.concat  "\n----------------------------\n")       
    
# filterOut =  \ log, matches  ->
#     workedOut = List.walk matches (Ok { whiteList : [],  blackList : [], regionsList : [] }) 
#         ( \ state , word  -> 
#             when state is 
#                 Ok stateInt ->
#                     when regionKey word is
#                         Ok res  ->
#                             when  res  is
#                                 Region val ->
#                                         Ok { stateInt & regionsList: List.append stateInt.regionsList val  }
#                                 Simple str ->
#                                     if Str.startsWith word "@" == Bool.true then
#                                         Ok { stateInt & blackList:   List.append stateInt.blackList  (Str.replaceFirst  str "@"  "" ) }
#                                     else
#                                         Ok { stateInt & whiteList: List.append stateInt.whiteList str   }
#                         Err _ -> Err word   
#                 Err _ -> Err word       
#             )
    
#     when workedOut is
#         Ok tokens ->
#             Ok (filterInternal log  tokens.whiteList  Bool.true
#             |>  filterInternal tokens.blackList  Bool.false
#             |> filterRegion tokens.regionsList )
#         Err wrongToken -> Err  wrongToken 

# extractPat = \ lst ->
#     when  List.first lst  is 
#         Ok firstStr ->
#             when  List.last  lst  is
#                 Ok secStr -> 
#                     when Str.toNat  secStr  is 
                    
#                         Ok num -> Ok (Region { pat : firstStr, lines : num })
#                         Err _ -> Err PatternError    
#                 Err _ -> Err PatternError   
          
#         Err _ -> Err PatternError


# regionKey = \ str ->
#     items = Str.split  str "^"
#     when List.len items is
#         1 -> Ok  ( Simple  str )
#         2 -> extractPat items
#         _ -> Err  PatternError

app "peek"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br"
    }
    imports [
        pf.Stdin,
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Path,
        pf.Task.{Task},
        Regex,
        Commands.{PatternType},
        Utils]
    provides [main] to pf

# some operations will be so slow : ) so up to user it will be to decide which one
# to use in given  circumstances
LineSearchType : [ Hit PatternType, Miss ]

LineAnalyzedType : { content : List  Str, separator : List  Str }

LineCumulatedType : { number : U32 , line : LineAnalyzedType, status : LineSearchType }

#{  }

#analyseLine : U32, Str, List PatternType, List LineRegisterType -> Result (List LineRegisterType) Str   
#analyseLine = \ lineNumber, line, patterns, register ->
#    List.walk patterns 

plainWordMatch : Str, Str -> LineAnalyzedType
plainWordMatch = \ word, pattern ->
    when (Str.split  word  pattern ) is
        [one] ->
            { content : [word], separator : [] }
            
        lst -> 
            { content : lst, separator : List.repeat  pattern ((List.len lst) - 1) }


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


# [head, .. as tail]

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

wordMatches = \ word, patterns ->
    List.walk patterns Bool.false ( \flag, pat -> 
        if flag == Bool.true then
            Bool.true
        else
            when (Str.splitFirst  word  pat ) is
            Ok _ -> Bool.true
            Err  _ ->  Bool.false)


filterInternal = \ str, matches, inFlag  ->
    if List.isEmpty matches == Bool.true  then
        str
    else
        Str.split str "\n"
        |> List.walk "" ( \ state , elem ->  
            checkResult = wordMatches elem  matches
            if  (checkResult == inFlag) && (Str.isEmpty (Str.trim elem ) == Bool.false) then 
                Str.concat  state elem
                |>  Str.concat  "\n"
            else 
                state
            )


wordMatchesRegion = \ word, patterns ->
    List.walk patterns { matchResult : Bool.false, pattern :{ pat : "", lines : 0 }  } ( \state, pattern -> 
        when (Str.splitFirst  word  pattern.pat ) is
                Ok _ ->
                    if (state.matchResult == Bool.true) && (state.pattern.lines >= pattern.lines ) then
                        state
                    else                        
                        { state &  matchResult : Bool.true, pattern : pattern }
                Err  _ ->  state  )

filterRegion = \ inLog, regions  -> 
    if List.len  regions == 0 then
        inLog
    else 
        longest = List.walk regions 0  
            ( \ state , pat  -> 
                if pat.lines > state then
                    pat.lines
                else
                    state )
       
        refreashedRegions = regions
        
        getSizeLast = ( \ listOfLst  ->
            when List.last listOfLst is 
                Ok lst -> List.len lst
                Err _ -> 0 )
    
        nextLineParse = (\  state, line ->
            parsingResult = wordMatchesRegion line  refreashedRegions

            if parsingResult.matchResult == Bool.true  then  

                state
                |> ( \ stateIn ->

                    if parsingResult.pattern.lines > (getSizeLast stateIn.bufferMatched) then
                        { stateIn &
                            bufferMatched : 
                                List.dropLast  stateIn.bufferMatched 1
                                |> List.append  ( List.sublist stateIn.buffer { start: (longest - parsingResult.pattern.lines), len: parsingResult.pattern.lines } ) }
                    else
                        { stateIn &bufferMatched : 
                            (
                                when  List.last stateIn.bufferMatched is
                                    Ok lst -> 
                                        (List.replace stateIn.bufferMatched (List.len stateIn.bufferMatched - 1)  (List.append lst line)).list
                                    Err _ ->  List.append stateIn.bufferMatched [line] ) })
                |> ( \ modState ->     
                        if parsingResult.pattern.lines > modState.cnt  then
                            {modState & cnt : parsingResult.pattern.lines }
                        else
                            {modState & cnt : modState.cnt - 1  })
            else 
                if state.cnt > 0 then
                    {state &
                        cnt : state.cnt - 1,
                        bufferMatched : 
                            (
                                when  List.last state.bufferMatched is
                                    Ok lst -> 
                                        (List.replace state.bufferMatched (List.len state.bufferMatched - 1)  (List.append lst line)).list
                                    Err _ ->  List.append state.bufferMatched [line] ) }
                else
                    when  List.last state.bufferMatched is
                        Ok lst -> 
                            if List.len lst > 0 then
                            {state &
                                bufferMatched :  List.append state.bufferMatched  [] }
                            else
                                state
                        Err _ -> state )
                    
        analysysDone = 
            Str.split inLog "\n"
            |> List.walk { cnt : 0, buffer : [], bufferMatched : []}
                (\ state , line ->
                    
                    state 
                    |> ( \ inState ->
                        if List.len inState.buffer < longest then
                            { inState & buffer :  List.append inState.buffer line}
                        else
                            { inState & buffer : ( 
                                List.dropFirst inState.buffer 1
                                |> List.append  line  )} )
                    |> nextLineParse line 
                )
            
        
        List.walk analysysDone.bufferMatched ""  
            ( \ state, lst ->  
                if List.isEmpty lst  == Bool.true then
                    state
                else              
                    List.walk lst state ( \inState , line ->
                        Str.concat  inState  "\n"
                        |> Str.concat line ) 
                    |>Str.concat  "\n----------------------------\n")       
    
    

filterOut =  \ log, matches  ->
    workedOut = List.walk matches (Ok { whiteList : [],  blackList : [], regionsList : [] }) 
        ( \ state , word  -> 
            when state is 
                Ok stateInt ->
                    when regionKey word is
                        Ok res  ->
                            when  res  is
                                Region val ->
                                        Ok { stateInt & regionsList: List.append stateInt.regionsList val  }
                                Simple str ->
                                    if Str.startsWith word "@" == Bool.true then
                                        Ok { stateInt & blackList:   List.append stateInt.blackList  (Str.replaceFirst  str "@"  "" ) }
                                    else
                                        Ok { stateInt & whiteList: List.append stateInt.whiteList str   }

                        Err _ -> Err word   
                Err _ -> Err word       
            )
    
    when workedOut is
        Ok tokens ->
            Ok (filterInternal log  tokens.whiteList  Bool.true
            |>  filterInternal tokens.blackList  Bool.false
            |> filterRegion tokens.regionsList )
        Err wrongToken -> Err  wrongToken 

extractPat = \ lst ->
    when  List.first lst  is 
        Ok firstStr ->
            when  List.last  lst  is
                Ok secStr -> 
                    when Str.toNat  secStr  is 
                    
                        Ok num -> Ok (Region { pat : firstStr, lines : num })
                        Err _ -> Err PatternError    
                Err _ -> Err PatternError   
          
        Err _ -> Err PatternError


regionKey = \ str ->
    items = Str.split  str "^"
    when List.len items is
        1 -> Ok  ( Simple  str )
        2 -> extractPat items
        _ -> Err  PatternError


main =
    #read =
    #    path = Path.fromStr "log.txt"
    #    out = Path.fromStr "out.txt"

    dbg mergeColors
        [
            { content : ["pomidor"], separator : [] },
            { content : ["po","d", ""], separator : ["mi","or"] },
            { content : ["p", "or"], separator : ["omid"] },
        ]

    Stdout.write "Ok\n"
        #hien <- File.readUtf8 path  |> Task.await
        #text <- Stdin.line |> Task.await 
        #    when text  is 
        #        Input  str -> Stdout.write "user entered \(str)\n"
        #        End -> Stdout.write "\n" 
        #loopTask =
        #    _ <- Stdout.write "provide your filters: " |> Task.await
        #    text <- Stdin.line  Task.await 
        #    filterResult = (filterOut hien (Utils.tokenize text) )
        #    when filterResult is
        #        Ok result ->
        #            _ <- File.writeUtf8 out result |> Task.await
        #            _ <- Stdout.write "\n" |> Task.await
        #            _ <- Stdout.write result |> Task.await
        #            _ <- Stdout.write "\nLast you used: \(text)" |> Task.await
        #            Stdout.write "\nload data to out.txt" 
        #        Err wrongToken ->
        #            Stdout.write "\nthere is mistake in your filter: \(wrongToken)\n Try again:" 
        
        #Task.loop {} \_ -> Task.map loopTask Step

    
    #Task.attempt read \result ->
    #    when result is
    #        Ok {} -> Stdout.write "Successfully wrote a string to out.txt"
    #        Err err ->
    #            msg =
    #                when err is
    #                    FileWriteErr _ PermissionDenied -> "PermissionDenied"
    #                    FileWriteErr _ Unsupported -> "Unsupported"
    #                    FileWriteErr _ (Unrecognized _ other) -> other
    #                    FileReadErr _ _ -> "Error reading file"
    #                    _ -> "Uh oh, there was an error!"

     #           {} <- Stderr.line msg |> Task.await
     #           Task.ok  {}
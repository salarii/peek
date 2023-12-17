app "peek"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdin,
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Path,
        pf.Task.{ Task }]
    provides [main] to pf


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
                                List.dropLast  stateIn.bufferMatched
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
                                List.dropFirst inState.buffer
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
    read =
        path = Path.fromStr "log.txt"
        out = Path.fromStr "out.txt"
        hien <- File.readUtf8 path  |> Task.await
        
        loopTask =
            _ <- Stdout.line "provide your filters: " |> Task.await
            text <- Task.await Stdin.line
            filterResult = (filterOut hien (tokenize text) )
            when filterResult is
                Ok result ->
                    _ <- File.writeUtf8 out result |> Task.await
                    _ <- Stdout.line "\n" |> Task.await
                    _ <- Stdout.line result |> Task.await
                    _ <- Stdout.line "\nLast you used: \(text)" |> Task.await
                    Stdout.line "\nload data to out.txt" 
                Err wrongToken ->
                    Stdout.line "\nthere is mistake in your filter: \(wrongToken)\n Try again:" 
        
        Task.loop {} \_ -> Task.map loopTask Step

    
    Task.attempt read \result ->
        when result is
            Ok {} -> Stdout.line "Successfully wrote a string to out.txt"
            Err err ->
                msg =
                    when err is
                        FileWriteErr _ PermissionDenied -> "PermissionDenied"
                        FileWriteErr _ Unsupported -> "Unsupported"
                        FileWriteErr _ (Unrecognized _ other) -> other
                        FileReadErr _ _ -> "Error reading file"
                        _ -> "Uh oh, there was an error!"

                {} <- Stderr.line msg |> Task.await
                Task.ok  {}
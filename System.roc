
interface  System
    exposes [
        executeCommand,
        updateData,
        guessPath,
        grouping,
        printGroup,
        printGroupWithSet,
        setupHistory,
        storeHistory,
        checkListOfDirsGiveSet,
        stripPath,
        listEntries]

    imports [
        pf.Stdin, 
        pf.Stdout, 
        pf.Task.{ Task, await, loop },
        pf.Cmd,
        pf.Cmd.{Cmd},
        pf.Cmd.{Output },
        pf.Path,
        pf.Dir,
        pf.File,
        pf.Env,
        Utils,
        State,
        State.{StateType,AppModeType}
        ]

# the goal of embeded terminal is to just facilitate navigation between locations
# ls, cd stuff like that should work 
# I want to be able to locate and load log files for further processing 
# maybe some other simple operations nothing more (piping, bash scripts, I don't know how to do and I don't want even try )

GuessEffectType : [Extend Str, ListDir (List Str), None]

executeCommand : StateType, List Str -> Task StateType *
executeCommand = \ state, lstCmd ->

    formatLsType : Str, Set Str -> Str
    formatLsType = \ out, dirInfo ->
        if Set.isEmpty  dirInfo then
                Utils.tokenizeNewLine out
                |> grouping  4 100
                |> printGroup 
        else
                Utils.tokenizeNewLine out
                |> grouping  4 100
                |> printGroupWithSet dirInfo
            
    execute : Cmd, Task ( Bool, Str ) * -> Task StateType *
    execute = \ command, lsFormat ->
        result <- Cmd.output  command |> Task.attempt
            when result is  
                Ok out ->
                    lsFormatResult <- lsFormat |> Task.attempt
                    when lsFormatResult is 
                    Ok format -> 
                        if format.0 == Bool.true then
                            lsOut = (Utils.utfToStr out.stdout)
                            if Str.isEmpty format.1 then
                                Task.ok  (State.setCommandOutput state (formatLsType lsOut (Set.empty {} )) )   
                            else 
                                tokenized = Utils.tokenizeNewLine lsOut

                                tokenized
                                |> List.walk (Task.ok (Set.empty {})) (\ dirSearch, line ->
                                    when  Utils.tokenize line is 
                                        [entry] ->
                                            dirLstResult <- dirSearch |> Task.attempt
                                            # assume // will work as "/"
                                            path =
                                                Str.concat format.1 "/"
                                                |>Str.concat entry
                                            isDir <- isDirectoryPath path |> Task.attempt
                                            when dirLstResult is
                                                Ok  dirSet ->
                                                    if (Result.withDefault isDir Bool.false ) == Bool.true then
                                                        Task.ok (Set.insert dirSet entry)
                                                    else
                                                         Task.ok dirSet 
                                                Err _ -> dirSearch 
                                        _ -> dirSearch )
                                |> ( \ allDirs -> 
                                    dirs <- allDirs |> Task.attempt
                                    colorInfo = (Result.withDefault dirs  (Set.empty {}))
                                    Task.ok  (State.setCommandOutput state (formatLsType lsOut colorInfo))
                                    )  
                        else
                            Task.ok  (State.setCommandOutput state (Utils.utfToStr out.stdout))
                    Err _ ->         
                        Task.ok  (State.setCommandOutput state (Utils.utfToStr out.stdout)) 

                        
                Err (out,err) ->
                    Task.ok  (State.setCommandOutput state (Utils.utfToStr out.stderr))
    
    executeCd : Str -> Task StateType *
    executeCd = \ path ->
        if Str.isEmpty path == Bool.true then
            
            systemData = State.getSystemData state
            setEnv <-Env.setCwd (Path.fromStr systemData.homePath) |> Task.attempt
            when setEnv is
                Ok _ -> updateData state
                Err _ -> Task.ok state

        else
            isDirResult <-isDirectoryPath path |> Task.attempt
            when isDirResult is
                Ok isDir ->
                    if isDir == Bool.true then
                        setEnv <-Env.setCwd (Path.fromStr path) |> Task.attempt
                        when setEnv is
                            Ok _ ->
                                updateData state
                            Err _ ->
                                Task.ok state
                    else
                        Task.ok state
                Err _ -> Task.ok state
                
    analyzeLsData : List Str, Bool -> Task ( Bool, Str) *
    analyzeLsData = \ args, isLs ->
        if isLs == Bool.true then 
            when args is 
                [] ->
                    Task.ok (Bool.true, ".")
                _ ->
                    # I know it is weird but things like that should work,
                    # and they don't. This is kind of weakness of this Task concept. (maybe is just me but working, tasks <-> other stuff, is difficult ) 
                    #pathResult <- List.walkUntil args ( Task.ok "")  (\ _state, arg ->
                    #            isDir <- isDirectoryPath arg |> Task.attempt
                    #            if (Result.withDefault isDir Bool.false ) == Bool.true then 
                    #                Break (Task.ok arg)
                    #            else 
                    #                Continue (Task.ok "")
                    #        )
                    #    |> Task.attempt
                    pathResult <- List.walk args ( Task.ok "")  (\ spoted, arg ->
                                isEnd <- spoted |> Task.attempt 
                                if Str.isEmpty (Result.withDefault isEnd "") == Bool.false then
                                    spoted
                                else
                                    isDir <- isDirectoryPath arg |> Task.attempt
                                    if (Result.withDefault isDir Bool.false ) == Bool.true then 
                                        Task.ok arg
                                    else 
                                        Task.ok ""
                            )
                        |> Task.attempt
                    Task.ok (Bool.true, Result.withDefault pathResult "")
        else 
            Task.ok (Bool.false, "")

    when lstCmd is 
        [] -> Task.ok state
        [name] ->
            if name == "cd" then
                executeCd "" 
            else
                command =
                    Cmd.new name
                execute command (analyzeLsData [] (name == "ls"))
        [name, .. as args] ->
            if name == "cd" then
                when args is 
                    [arg] -> executeCd arg
                    _ -> Task.ok state
            else
                command =
                    Cmd.new name
                    |> Cmd.args args
                execute command (analyzeLsData args (name == "ls"))

guessPath : Str -> Task GuessEffectType *
guessPath = \ path ->
    listedTop <- listEntries path |> Task.attempt
    when listedTop is 
        Ok listed ->
            if List.isEmpty listed then
                when stripPath path is 
                    Ok splitted -> 
                        listedBase <- listEntries splitted.before |> Task.attempt
                        when listedBase is 
                            Ok base ->
                                if List.isEmpty base then
                                    Task.ok None
                                else
                                    cleanPath =
                                        List.map base (\ subPath -> 
                                             when Str.splitLast subPath "/" is 
                                                Ok splited -> splited.after
                                                Err _ -> subPath
                                            )

                                    Task.ok (guessEffect cleanPath splitted.after)
                            Err _ -> Task.ok None
                    Err _ -> Task.ok None
            else
                when Str.toUtf8 path is 
                    [..,'/'] -> 
                        Task.ok (ListDir listed)
                    _ ->
                        Task.ok (Extend "/")
        Err _ -> Task.ok None

isDirectoryPath : Str -> Task Bool  *
isDirectoryPath = \ str ->
    command =
        Cmd.new "test"
        |> Cmd.args ["-d",  str]
        
    result <- Cmd.status  command |> Task.attempt
    when result is  
        Ok out ->
            Task.ok Bool.true
        Err _ ->
            Task.ok Bool.false

listEntries : Str -> Task (List Str) *
listEntries = \ path -> 
    accessResult <-Dir.list  (Path.fromStr path) |> Task.attempt
    when accessResult is
         Ok fileLst ->

            List.walk fileLst [] (\ state, item->
                List.append  state (Path.display item)
            )
            |> Task.ok 
         Err err -> Task.ok [] 
            
updateData : StateType -> Task StateType *
updateData = \ state -> 
    currentResult <- Env.cwd  |>  Task.attempt
    when currentResult is 
        Ok current ->
            res <-Env.var "HOME" |>  Task.attempt
            when res is 
                Ok homeDir ->
                    Task.ok (
                        (State.setSystemData 
                            state
                            { homePath : homeDir, current : Path.display current }
                        )
                        |> State.updatePrompt
                    )
                Err _ -> 
                    Task.ok state
        Err _ -> Task.ok state
    

grouping : List Str, Nat, Nat ->  { content: List Str, colCnt : Nat }
grouping = \ textLst, seaparatorLen, lineSize ->

    findGroup : Nat -> { content: List Str, colCnt : Nat }
    findGroup = \ colCnt ->
        if colCnt == 4 || colCnt == 3 || colCnt == 2 then
            grouped =
                List.walk  textLst ( List.repeat [] colCnt ) ( \state, item->
                    when state is
                        [col1, col2, col3, col4] ->
                            if List.len col1 > List.len col2 then
                                [col1, (List.append col2 item), col3, col4]
                            else if List.len col2 > List.len col3 then
                                [col1, col2, (List.append col3 item), col4]
                            else if List.len col3 > List.len col4 then
                                [col1, col2, col3, (List.append col4 item)]
                            else
                                [(List.append col1 item), col2, col3, col4]
                        [col1, col2, col3] ->
                            if List.len col1 > List.len col2 then
                                [col1, (List.append col2 item), col3]
                            else if List.len col2 > List.len col3 then
                                [col1, col2, (List.append col3 item)]
                            else
                                [(List.append col1 item), col2, col3]
                        [col1, col2] ->
                            if List.len col1 > List.len col2 then
                                [col1, (List.append col2 item)]
                            else
                                [(List.append col1 item), col2]
                        [col] -> [List.append col item]
                        _ -> [])
            
            List.map grouped (\ lst ->
                
                List.walk lst 0 ( \ len,  word ->
                    currentLen = List.len (Str.toUtf8 word)
                    if currentLen > len then
                        currentLen
                    else
                        len ))
            |> (\ lenArr ->
                if ((List.walk lenArr 0 (\ len, elem -> len + elem ))
                    + colCnt * seaparatorLen) > lineSize then
                    findGroup (colCnt - 1)
                else 
                    List.map2 grouped lenArr (\ group, len ->
                            List.map group ( \ word ->
                                Str.concat word  (Str.repeat " " (len + seaparatorLen - (List.len (Str.toUtf8 word)) ) )
                    ))
                    |> (\ adjusted -> 
                            gather = \ current, set ->
                                when current is 
                                    [col1,..] ->
                                        if List.isEmpty col1 == Bool.true then
                                            set
                                        else
                                            updatedSet = List.walk current set ( \ state, lst ->
                                                when lst is 
                                                    [head,..] -> List.append state head
                                                    [] -> state ) 
                                            sliced = List.map current (\ lst ->
                                                List.dropFirst lst 1)
                                            gather  sliced  updatedSet
                                             
                                    _-> textLst
                            gather adjusted  []
                        )
                    |> ( \ updatedContent ->
                        { content: updatedContent, colCnt : colCnt }))
        else
            { content: textLst, colCnt : 1 }
    findGroup 4

checkListOfDirsGiveSet : List Str -> Task (Set Str) *
checkListOfDirsGiveSet = \ dirs ->
    onlyDirsTask =
            (List.walk dirs (Task.ok []) (\state, dir ->
                dirPresence <- state |> Task.attempt
                result <- isDirectoryPath dir |> Task.attempt
                if Result.withDefault result Bool.false == Bool.true then
                    Task.ok (List.append (Result.withDefault dirPresence [])  dir)
                else
                    Task.ok (Result.withDefault dirPresence [])
            ))
        # this code  enters infinite   loop bug??
        #List.walk dirs (Task.ok (Set.empty {})) (\state, dir ->
        #    dirPresence <- state |> Task.attempt
        #    result <- isDirectoryPath dir |> Task.attempt
        #    if Result.withDefault result Bool.false == Bool.true then
        #        Task.ok (Set.insert (Result.withDefault dirPresence (Set.empty {}))  dir)
        #    else
        #        state
        #)

    onlyDirsResult<-onlyDirsTask |> Task.attempt
    Task.ok ( Set.fromList (Result.withDefault onlyDirsResult []) )
     

printGroup :  { content: List Str, colCnt : Nat } -> Str
printGroup = \ group -> 
    List.walk group.content ("",0) (\state, word ->
        if state.1 == group.colCnt - 1 then
            (Str.concat state.0 (Str.concat word "\n\r"), 0 )
        else
            (Str.concat state.0 word, state.1 + 1 )
    )
    |>( \ result -> Str.concat result.0 "\n\r")

printGroupWithSet :  { content: List Str, colCnt : Nat }, Set Str -> Str
printGroupWithSet = \ group, set -> 
    List.map group.content  ( \ word ->
        if Set.contains set (Str.trimEnd word)  == Bool.true then
            Utils.withColor word Red
        else
            word
    ) 
    |> List.walk  ("",0) (\state, word ->
        if state.1 == group.colCnt - 1 then
            (Str.concat state.0 (Str.concat word "\n\r"), 0 )
        else
            (Str.concat state.0 word, state.1 + 1 )
    )
    |>( \ result -> Str.concat result.0 "\n\r")

directoryMap : Str, List Str -> Task (List Bool)  *
directoryMap = \ path, items ->
    List.map items (\ item ->
        path
        |> Str.concat "/"
        |> Str.concat item )
    |> List.walk (Task.ok []) ( \ state, itemPath ->
        lst <- state |> Task.await  
        isDirectoryResult <- isDirectoryPath itemPath |> Task.attempt  
        when isDirectoryResult is 
           Ok isDirectory -> Task.ok (List.append lst isDirectory )
           Err _ -> Task.ok (List.append lst Bool.false)
        )

stripPath : Str -> Result {before : Str, after: Str}  Str
stripPath = \path ->
    when Str.splitLast path "/" is 
        Ok splited -> 
            Ok {splited & before : Str.concat splited.before "/" }
        Err _ -> Ok {before : "./", after:path }

guessEffect : List Str, Str -> GuessEffectType
guessEffect = \ lst, pattern  ->
    if Str.isEmpty pattern == Bool.true then
        None
    else 
        when findSubset lst pattern is 
            [] -> None
            subset -> 
                if List.contains subset "" == Bool.true then 
                    ListDir subset
                else
                    List.map subset ( \ word ->
                        when Str.splitFirst word pattern is 
                            Ok splited -> splited.after
                            Err _ -> word
                    )
                    |> findCommon 
                    |> ( \ common ->
                        if Str.isEmpty common == Bool.true then 
                            ListDir subset
                        else
                            Extend common
                    )

findSubset : List Str, Str -> List Str
findSubset = \ lst, pattern ->
    List.walk lst  [] ( \ subset, word -> 

        if Str.startsWith word pattern == Bool.true then
            List.append subset word 
        else
            subset
    )

findCommon : List Str -> Str
findCommon = \ lst -> 
    List.map lst  ( \ word -> Str.toUtf8 word )
    |> List.sortWith ( \ left, right ->
        if List.len left > List.len right then
            GT
        else if List.len left == List.len right then
            EQ
        else  
            LT) 
    |> (\ utfLst ->
        when utfLst is 
            [first, .. as tail ] ->
                List.walk tail first (\ current, newLst ->
                    List.map2 current newLst ( \ lChar, rChar ->
                        if lChar == rChar then
                            lChar
                        else
                            0 ))
                |> List.walkUntil "" ( \ common,char -> 
                    if char != 0 then
                        Continue ( Str.concat common ( Utils.utfToStr [char] ))
                    else 
                        Break common )
            [] -> ""
          ) 


isFilePath : Str -> Task Bool  *
isFilePath = \ str ->
    command =
        Cmd.new "test"
        |> Cmd.args ["-f",  str]
        
    result <- Cmd.status  command |> Task.attempt
    when result is  
        Ok out ->
            Task.ok Bool.true
        Err _ ->
            Task.ok Bool.false
            
systemFile : Str
systemFile = "/sys"

searchFile : Str
searchFile = "/search"

switchHistory : StateType, AppModeType -> StateType
switchHistory = \ state, mode -> 
    currentHistory = (State.getTerminalState state).commandHistory
    comHistory = State.getCommandHistory state
    when mode is 
        System -> 
            State.setCommandHistory state {comHistory & search : currentHistory}
            |> State.setTerminalHistory comHistory.sys
        Search -> 
            State.setCommandHistory state {comHistory & sys : currentHistory}
            |> State.setTerminalHistory comHistory.search
        _ -> state  
             
setupHistory : StateType -> Task StateType *
setupHistory = \ state -> 
    manipulateStore state setupHistoryInternal
    
storeHistory : StateType -> Task StateType *
storeHistory = \ state -> 
    manipulateStore state storeHistoryInternal
    
storeHistoryInternal : StateType, Str -> Task StateType *
storeHistoryInternal = \ state, storePath -> 
    sytemTermHistory = Str.concat storePath systemFile
    searchTermHistory = Str.concat storePath searchFile
    mode = State.getAppMode state
    toStore =
        (State.getTerminalState state).commandHistory
        |> List.map ( \ cmd -> Utils.utfToStr cmd)
        |> List.walk "" (\ out, cmd ->
            Str.concat out  cmd 
            |> Str.concat "\n" )
    
    when mode is 
        System -> 
            _<-File.writeUtf8 (Path.fromStr sytemTermHistory) toStore |> Task.attempt
            Task.ok state
        Search ->
            _<-File.writeUtf8 (Path.fromStr searchTermHistory) toStore |> Task.attempt
            Task.ok state
        _ -> Task.ok  state   
    
setupHistoryInternal : StateType, Str -> Task StateType *
setupHistoryInternal = \ state, storePath -> 
    sytemTermHistory = Str.concat storePath systemFile
    searchTermHistory = Str.concat storePath searchFile

    sysResult <- File.readUtf8 (Path.fromStr sytemTermHistory) |> Task.attempt
    sysHistory =
        when sysResult is
            Ok sys ->
                
                Utils.tokenizeNewLine sys
                |> List.map ( \ com ->  Str.toUtf8 com )
            Err _ -> []
    searchResult <- File.readUtf8 (Path.fromStr searchTermHistory) |> Task.attempt
    searchHistory =
        when searchResult is
            Ok search ->
                
                Utils.tokenizeNewLine search
                |> List.map ( \ com ->  Str.toUtf8 com )
            Err _ -> []
    Task.ok ( State.setCommandHistory  state {sys : sysHistory, search : searchHistory} )
    
manipulateStore : StateType, (StateType, Str-> Task StateType a) -> Task StateType a
manipulateStore = \ state, operation ->
    homePath = (State.getSystemData state).homePath
    if Str.isEmpty homePath == Bool.false then
        storeDir = Str.concat homePath "/.peek"
        isDirectoryResult <-isDirectoryPath storeDir |>  Task.attempt
        when isDirectoryResult is 
            Ok isDirectory ->
                if isDirectory == Bool.true then
                    operation state storeDir
                else 
                    _<-Dir.create (Path.fromStr storeDir) |> Task.attempt
                    Task.ok state
            Err _ ->
                Task.ok state
        
    else 
        Task.ok state


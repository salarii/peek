# app "system"
#     packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    
interface  System
    exposes [executeCommand,updateData,guessPath, grouping, printGroup]
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
        State.{StateType}
        ]
    # provides [main] to pf

GuessEffectType : [Extend Str, ListDir (List Str), None]

executeCommand : StateType -> Task StateType *
executeCommand = \ state ->
    lstCmd = Utils.tokenize (State.getCommand  state)
    
    formatLsType : Str -> Str
    formatLsType = \ out -> 
        Utils.tokenizeNewLine out
        |> grouping  4 100
        |> printGroup   
    
    execute : Cmd, Bool -> Task StateType *
    execute = \ command, lsFormat ->
        result <- Cmd.output  command |> Task.attempt
            when result is  
                Ok out ->
                    if lsFormat == Bool.true then 
                        Task.ok  (State.setCommandOutput (formatLsType (Utils.utfToStr out.stdout)) state)
                    else 
                        Task.ok  (State.setCommandOutput (Utils.utfToStr out.stdout) state)
                Err (out,err) ->
                    Task.ok  (State.setCommandOutput (Utils.utfToStr out.stderr) state)
    
    
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

    when lstCmd is 
        [] -> Task.ok state
        [name] ->
            if name == "cd" then
                executeCd ""
            else
                command =
                    Cmd.new name
                execute command (name =="ls")
        [name, .. as args] ->
            if name == "cd" then
                when args is 
                    [arg] -> executeCd arg
                    _ -> Task.ok state
            else
                command =
                    Cmd.new name
                    |> Cmd.args args
                execute command (name =="ls")

guessPath : Str -> Task GuessEffectType *
guessPath = \ path ->
    listedTop <- listFiles path |> Task.attempt
    when listedTop is 
        Ok listed ->
            if List.isEmpty listed then
                when stripPath path is 
                    Ok splitted -> 
                        listedBase <- listFiles splitted.before |> Task.attempt
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
    listResult <-listFiles str |> Task.attempt
    when listResult is
        Ok fileLst ->
            Task.ok   (Bool.not (List.isEmpty fileLst))
        Err err -> Task.ok  Bool.false

listFiles : Str -> Task (List Str) *
listFiles = \ path -> 
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
                        State.setSystemData 
                            { homePath : homeDir, current : Path.display current }
                            state
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

printGroup :  { content: List Str, colCnt : Nat } -> Str
printGroup = \ group -> 
    List.walk group.content ("",0) (\state, word ->
        if state.1 == group.colCnt - 1 then
            (Str.concat state.0 (Str.concat word "\n\r"), 0 )
        else
            (Str.concat state.0 word, state.1 + 1 )
    )
    |>( \ result -> Str.concat result.0 "\n\r")

printGroupWithMap :  { content: List Str, colCnt : Nat }, List Bool -> Str
printGroupWithMap = \ group, map -> 
    List.map2 group.content map ( \ word, color ->
        if color == Bool.true then
            Utils.withColor word Red
        else
            word
    ) 
    |> List.walk  ("",0) (\state, word ->
        if state.1 == group.colCnt - 1 then
            (Str.concat state.0 (Str.concat word "\n"), 0 )
        else
            (Str.concat state.0 word, state.1 + 1 )
    )
    |>( \ result -> Str.concat result.0 "\n")

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

#main =
    # command =
    #     Cmd.new "test"
    #     |> Cmd.args ["-f "]

    #g  = grouping (List.repeat "japko" 20 |> List.append "tttttttddddddfsdf")  2 40

    # stuff = "afsdfsf"

    

    
    



    
    

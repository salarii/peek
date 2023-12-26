interface  Terminal
    exposes [step,init,setCursor,displayCommand]
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task.{ Task },
        #pf.Tty,  for  some  reason this  import  does not work
        Regex,
        Utils,
        State,
        Commands,
        Commands.{quitCommand},
        State.{StateType,TerminalLineStateType}
    ]

initPhrase : List U8
initPhrase = Str.toUtf8 ""

addToHistoryList : List (List U8), List U8 -> List (List U8)
addToHistoryList = \ historyLst, newItem ->
    if List.len newItem > 0 then
        List.walk historyLst [] ( \ state, item  ->
            if item == newItem then
                state
            else
                List.append state item
        )
        |> List.prepend  newItem
    else
        historyLst

addToHistoryListNoAlter : List (List U8), List U8 -> List (List U8)
addToHistoryListNoAlter = \ historyLst, newItem -> 
    if List.len newItem > 0 then
        insert =
            List.walkUntil historyLst Bool.true ( \ state, item  ->
                if item == newItem then
                    Break Bool.false
                else
                    Continue state
            )
        if insert == Bool.true then 
            List.prepend historyLst newItem
        else
            historyLst 
    else
        historyLst

injectString : List U8, List U8, I32 -> {composed : List U8,inJectedCnt : I32 } 
injectString =  \ destStr, inStr, n -> 
    List.split destStr (Num.toNat n)
    |> (\ splited ->
        composed = 
            List.concat splited.before inStr
            |> List.concat splited.others
        { composed : composed, inJectedCnt : Num.toI32 (List.len inStr) } )   

removeCharString : List U8, I32 ->  List U8 
removeCharString =  \ destStr, n -> 
    List.split destStr (Num.toNat n)
    |> (\ splited ->
        List.dropLast  splited.before  1
        |> List.concat splited.others )

init : StateType -> Task StateType *
init = \ appState -> 
    {} <- Stdout.write homeLinePat |> Task.await
    setCursor appState

setCursor : StateType -> Task StateType *
setCursor = \ appState ->
    state =  State.getTerminalState appState
    setupTerminal =
        #{} <- Tty.enableRawMode |> Task.await
        #{} <- Stdout.write  clearScreenPat |> Task.await
        #{} <- Stdout.write  (cursorPosition state.cursor) |> Task.await
        #{} <- Stdout.write  (Utils.utfToStr state.content) |> Task.await
        {} <- Stdout.write  queryScreenPositionPat |> Task.await
        Stdin.bytes
    positionResult <- setupTerminal |> Task.attempt
    when positionResult is
        Ok position ->
            cursorPositionRes = queryPosition position 
            when cursorPositionRes is 
                Ok cursor ->
                    Task.ok (State.setTerminalState  {state & cursor : cursor} appState)
                Err _ -> Task.ok appState
        Err _ -> Task.ok appState

displayCommand :StateType-> Task StateType *
displayCommand = \ appState ->

    #{} <- Stdout.write clearLinePat |> Task.await
    out = State.getCommandOutput appState
    if List.isEmpty out == Bool.false then
        print =
            Task.loop out ( \ lst ->
                when lst is
                    [front, .. as back ] -> 
                        {} <- Stdout.write homeLinePat |> Task.await
                        {} <- Stdout.write front |> Task.await
                        {} <- Stdout.write "\n" |> Task.await
                        Task.ok (Step back)
                    [] -> 
                        Task.ok (Done {})
            )
        active = State.getCommand appState
        
        {} <- Stdout.write clearLinePat |> Task.await
        {} <- Stdout.write homeLinePat |> Task.await
        {} <- Stdout.write "Command executed: " |> Task.await
        {} <- Stdout.write active |> Task.await
        {} <- Stdout.write "\n\n" |> Task.await
        {} <- Stdout.write homeLinePat |> Task.await
        {} <- print |> Task.await
        {} <- Stdout.write clearLinePat |> Task.await
        _ <- Stdout.write homeLinePat |> Task.attempt
        cursorUpdated <-setCursor appState |> Task.await
        Task.ok cursorUpdated
    else
        Task.ok appState

step : StateType-> Task StateType *
step = \ appState ->
    state =  State.getTerminalState appState
    updateInputTask = 
        {} <- drawState state |> Task.await
        Stdin.bytes

    inputResult <- updateInputTask |> Task.attempt     
    when inputResult is 
        Ok input ->  
            command = parseRawStdin input
            when command is 
                Shift direction -> 
                    if (Num.toI32 (List.len state.content) < state.cursor.col && direction == (Right 1 )) ||
                        ((direction == (Left 1 )) && state.cursor.col == 1 ) then
                        Task.ok appState
                    else
                        Task.ok (State.setTerminalState (modifyCursor state direction) appState)
                Characters chars ->
                    Task.ok (State.setTerminalState (modifyLine state (Characters chars)) appState)
                RemoveLast ->
                    Task.ok (State.setTerminalState (modifyLine state RemoveLast) appState)
                ClearLine ->
                    Task.ok (State.setTerminalState (clearLine state) appState)
                PreviousCommand ->
                    Task.ok (State.setTerminalState (fromHistory state Previous) appState)
                EnterCommand ->
                    updatedState =
                        Commands.setupSystemCommand  (Utils.utfToStr state.content) appState
                    Task.ok (State.setTerminalState (enterHistory state) updatedState)
                NextCommand ->
                    Task.ok (State.setTerminalState (fromHistory state Next) appState)
                Quit -> 
                    Task.ok (Commands.setupSystemCommand quitCommand appState)
                Unsupported ->
                    Task.ok appState
                _ -> 
                    Task.ok appState
        Err _ -> Task.ok appState

modifyCursor : TerminalLineStateType, Direction -> TerminalLineStateType
modifyCursor = \state, direction ->
    when direction is 
        Left val-> 
            { state & 
                cursor: {
                    row: state.cursor.row, 
                    col: state.cursor.col - val,
                }
            }
        Right val->
            { state & 
                cursor: {
                    row: state.cursor.row, 
                    col: state.cursor.col + val,
                }
            }

        Begin ->
            { state & 
                cursor: {
                    row: state.cursor.row, 
                    col: 1,
                }
            }
        End ->
            { state & 
                cursor: {
                    row: state.cursor.row, 
                    col: Num.toI32 (List.len state.content)+1,
                }
            }

enterHistory : TerminalLineStateType -> TerminalLineStateType
enterHistory = \state ->
    {
        state &
        content : [],
        historyCnt:  -1,
        commandHistory :
            addToHistoryList state.commandHistory state.content,
        cursor :  {row : state.cursor.row, col : 1 }
    }

fromHistory : TerminalLineStateType, [Previous,Next] -> TerminalLineStateType
fromHistory = \state, order ->
    length = Num.toI32 (List.len state.commandHistory)
    if order == Previous && length - 1> state.historyCnt then 
            List.get state.commandHistory (Num.toNat (state.historyCnt+1))
            |>Result.withDefault  []
            |> ( \ updatedContent -> 
                {
                    state &
                    content : updatedContent,
                    historyCnt:  state.historyCnt + 1,
                    commandHistory :
                        addToHistoryListNoAlter state.commandHistory state.content,
                    cursor :  {row : state.cursor.row, col : Num.toI32 (List.len updatedContent ) + 1 }
                })
    else if order == Next && state.historyCnt >= 0 then
            if state.historyCnt == 0 then 

                {
                        state &
                        content : [],
                        historyCnt:   -1,
                        commandHistory :
                            addToHistoryListNoAlter state.commandHistory state.content,
                        cursor :  {row : state.cursor.row, col : 0 }
                }
            else
                List.get state.commandHistory (Num.toNat (state.historyCnt-1))
                |>Result.withDefault  []
                |> ( \ updatedContent -> 
                    {
                        state &
                        content : updatedContent,
                        historyCnt:  state.historyCnt - 1,
                        commandHistory :
                            addToHistoryListNoAlter state.commandHistory state.content,
                        cursor :  {row : state.cursor.row, col : Num.toI32 (List.len updatedContent ) + 1 }
                    })
    else 
        state

clearLine : TerminalLineStateType -> TerminalLineStateType
clearLine = \state ->

    updatedContent = 
        List.split state.content (Num.toNat (state.cursor.col) - 1)
        |> (\ splited ->
            splited.others )
    { state & 
        content: updatedContent
    }
    |> modifyCursor Begin

modifyLine : TerminalLineStateType, [Characters (List U8),RemoveLast ] -> TerminalLineStateType
modifyLine = \state, operation ->
    when operation is 
        Characters chars ->

            injected = injectString state.content chars (state.cursor.col-1) 
            { state & 
                content: injected.composed
            }
            |> modifyCursor (Right injected.inJectedCnt )
        RemoveLast -> 
            
            { state & 
                content: removeCharString state.content (state.cursor.col - 1)
            }
            |> modifyCursor (Left 1)


drawState : TerminalLineStateType -> Task {} *
drawState = \state ->
    {} <- Stdout.write clearLinePat |> Task.await
    {} <- Stdout.write homeLinePat |> Task.await
    {} <- Stdout.write (Result.withDefault (Str.fromUtf8 state.content ) "") |> Task.await
    Stdout.write (cursorPosition state.cursor)

queryPosition : List U8 -> Result { row : I32, col : I32 }  Str
queryPosition = \ consoleOut ->
    position = Result.withDefault (Str.fromUtf8 (List.dropFirst consoleOut 1)) "" 
    when Regex.parseStr position "(\\d+);(\\d+)R"  is
        Ok parsed ->
            when ((Regex.getValue [0] 0 parsed.captured),
                 (Regex.getValue [1] 0 parsed.captured)) is
                ( Ok rowStr, Ok colStr  )->
                    Ok 
                        {
                            row : Utils.asciiArrayToNumber rowStr Str.toI32,
                            col : Utils.asciiArrayToNumber colStr Str.toI32
                        }
                _ -> Err "can't parse cursor position"
        Err  message -> Err message

Direction : [Left I32, Right I32, Begin, End ]

Action : [
    Unsupported,
    PreviousCommand,
    NextCommand,
    Shift Direction,
    RemoveLast,
    ClearLine,
    EnterCommand,
    Characters (List  U8),
    Quit,
    Empty,
]

queryScreenPositionPat = "\u(001b)[6n"
clearScreenPat = "\u(001b)[2J"
clearLinePat ="\u(001b)[2K" 
homeLinePat  = "\u(001b)[0G"
endLinePat  = "\u(001b)[K"

cursorPosition : {row: I32, col: I32} -> Str 
cursorPosition = \{row, col} ->
    rowStr = row |> Num.toStr
    colStr = col |> Num.toStr

    "\u(001b)[\(rowStr);\(colStr)H"


withColor : Str, [Red, Green, Blue]-> Str
withColor = \str, color ->
    when color is 
        Red -> "\u(001b)[31m\(str)\u(001b)[0m"
        Green -> "\u(001b)[32m\(str)\u(001b)[0m"
        Blue -> "\u(001b)[34m\(str)\u(001b)[0m"

# I want it to be not customizable
parseRawStdin : List U8 -> Action
parseRawStdin = \bytes ->
    when bytes is 
        [27, 91, 65, ..] -> PreviousCommand
        [27, 91, 66, ..] -> NextCommand
        [27, 91, 67, ..] -> Shift (Right 1)
        [27, 91, 68, ..] -> Shift (Left 1)
        [8, ..] | [127, ..] -> RemoveLast
        [21, ..] -> ClearLine
        [13, ..] -> EnterCommand
        [27, 91, 72,..] -> Shift Begin
        [27, 91, 70,..] -> Shift End 
        [27, val, cal,  ..] -> Quit
        [3, ..] -> Quit
        other -> Characters other
        [] -> Empty

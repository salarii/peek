app "native-tui"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br"
        #pf: "../basic-cli/src/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Sleep,
        pf.Tty,
        pf.Task.{ Task },
        Regex,
        Utils,
    ]
    provides [main] to pf

 #   "\u(001b)[\(rowStr);\(colStr)H"
# # Query cursor position
# echo -en "\033[6n"

main = Task.onErr terminal \_ -> crash "unknown problem"

LineState : {
    commandHistory : List (List U8),
    historyCnt : I32,
    content : List U8,
    cursor : { row : I32, col : I32 },
}


content = "here  will be some  kind of text message \n\rasjeiogjaesiogjarodsgjoard\n\rjgoarjoegjsaemv.lsmndo\n\r"
tail = "\n\n"

initPhrase : List U8
initPhrase = Str.toUtf8 ""

init : LineState
init = {
    commandHistory : [],
    historyCnt : -1,
    content : initPhrase,
    cursor: { row: 1, col: 1},
}

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

terminal : Task {} *
terminal =
    
    {} <- Tty.enableRawMode |> Task.await

    {} <- Stdout.write  clearScreenPat |> Task.await
    {} <- Stdout.write (cursorPosition init.cursor) |> Task.await
    {} <- Stdout.write  content |> Task.await
    {} <- Stdout.write  tail |> Task.await
    {} <- Stdout.write  queryScreenPositionPat |> Task.await
    cursorBytes <- Stdin.bytes |> Task.await
    cursorPositionRes = queryPosition cursorBytes 
    when cursorPositionRes is 
            Ok cursor ->
                _ <- Task.loop {init &  cursor : cursor } (\state ->

                    # Sleep to limit frame rate
                    {} <- Sleep.millis 50 |> Task.await

                    {} <- drawState state |> Task.await
                    
                    
                    # Get user input
                    bytes <- Stdin.bytes |> Task.await
                    command = parseRawStdin bytes

                    when command is 
                        Shift direction -> 

                            if (Num.toI32 (List.len state.content) < state.cursor.col && direction == (Right 1 )) ||
                            ((direction == (Left 1 )) && state.cursor.col == 1 ) then
                                Task.ok (Step state)
                            else
                                Task.ok (Step (modifyCursor state direction))
                        Characters chars ->
                            Task.ok (Step (modifyLine state (Characters chars)))
                        RemoveLast ->
                            Task.ok (Step (modifyLine state RemoveLast))
                        ClearLine ->
                            Task.ok (Step (clearLine state))
                        PreviousCommand ->
                            Task.ok (Step (fromHistory state Previous))
                        EnterCommand ->
                            Task.ok (Step (enterHistory state))
                        NextCommand ->
                            Task.ok (Step (fromHistory state Next))
                        Quit -> 
                            Task.ok (Done state)
                        Unsupported ->

                            # Clear the screen
                            {} <- Stdout.write  clearScreenPat |> Task.await

                            dbg "problem"
                            dbg bytes

                            Task.ok (Done state)
                        _ -> 
                            dbg "no t done yet"
                            Task.ok (Step state)

                ) |> Task.await
                
                # Disable raw mode
                {} <- Tty.disableRawMode |> Task.await

                Task.ok {}
            Err _ -> Task.ok {}

modifyCursor : LineState, Direction -> LineState
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

enterHistory : LineState -> LineState
enterHistory = \state ->
    {
        state &
        content : [],
        historyCnt:  -1,
        commandHistory :
            addToHistoryList state.commandHistory state.content,
        cursor :  {row : state.cursor.row, col : 1 }
    }

fromHistory : LineState, [Previous,Next] -> LineState
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

clearLine : LineState -> LineState
clearLine = \state ->

    updatedContent = 
        List.split state.content (Num.toNat (state.cursor.col) - 1)
        |> (\ splited ->
            splited.others )
    { state & 
        content: updatedContent
    }
    |> modifyCursor Begin

modifyLine : LineState, [Characters (List U8),RemoveLast ] -> LineState
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


drawState : LineState -> Task {} *
drawState = \state ->
    #stuff  =
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
        other -> 
            Characters other

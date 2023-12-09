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
    ]
    provides [main] to pf

 #   "\u(001b)[\(rowStr);\(colStr)H"
# # Query cursor position
# echo -en "\033[6n"

main = Task.onErr terminal \_ -> crash "unknown problem"

LineState : {
    content : Str,
    cursor : { row : I32, col : I32 },
}

initPhrase : Str
initPhrase = "something to it"

init : LineState
init = {
    content : initPhrase,
    cursor: { row: 1, col: Num.toI32 (Str.countUtf8Bytes initPhrase)},
}

terminal : Task {} *
terminal =
    
    {} <- Tty.enableRawMode |> Task.await

    _ <- Task.loop init (\state ->

        # Sleep to limit frame rate
        {} <- Sleep.millis 50 |> Task.await

        {} <- drawState state |> Task.await

        
        # Get user input
        bytes <- Stdin.bytes |> Task.await
        command = parseRawStdin bytes

        when command is 
            Shift direction -> 
                Task.ok (Step (modifyCursor state direction))
            Characters chars ->
                Task.ok (Step (modifyLine state (Characters chars)))
            RemoveLast ->
                Task.ok (Step (modifyLine state RemoveLast))
            ClearLine ->
                Task.ok (Step (clearLine state))
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
                    col: Num.toI32 (Str.countUtf8Bytes state.content)+1,
                }
            }

clearLine : LineState -> LineState
clearLine = \state ->
    { state & 
        content: "" 
    }
    |> modifyCursor Begin

modifyLine : LineState, [Characters (List U8),RemoveLast ] -> LineState
modifyLine = \state, operation ->
    dropLast = \ str ->
        Str.toUtf8  str
        |> List.dropLast 1
        |> Str.fromUtf8  
        |> Result.withDefault ""
    when operation is 
        Characters chars ->
            { state & 
                content: Str.concat state.content  (Result.withDefault  (Str.fromUtf8  chars) "")  
            }
            |> modifyCursor (Right (Num.toI32 (Str.countUtf8Bytes (Result.withDefault  (Str.fromUtf8  chars) ""))))
        RemoveLast -> 
            { state & 
                content: dropLast state.content  
            }
            |> modifyCursor (Left 1)

drawState : LineState -> Task {} *
drawState = \state ->
    #stuff  =
    {} <- Stdout.write  clearScreenPat |> Task.await

    {} <- Stdout.write clearLinePat |> Task.await
    {} <- Stdout.write homeLinePat |> Task.await
    {} <- Stdout.write state.content |> Task.await
    Stdout.write (cursorPosition state.cursor)

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

clearScreenPat = "\u(001b)[2J"
clearLinePat ="\u(001b)[2K" 
homeLinePat  = "\u(001b)[0G"
endLinePat  = "\u(001b)[K"
#"\u001b[#{row};#{column}H"
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
        [8, ..] | [127, ..] -> 
            RemoveLast
        [21, ..] -> 
            ClearLine
        [27, 91, 72,..] -> 
            Shift Begin
        [27, 91, 70,..] -> 
            Shift End
        [27, val, cal,  ..] ->
            Quit
        [3, ..] -> Quit
        other -> 
            Characters other

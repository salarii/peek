app "peek"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdin, 
        pf.Stdout, 
        pf.Task.{ Task ,await, loop },
        pf.Cmd,
        pf.Cmd.{Output },
        pf.Sleep,
        pf.Tty,
        pf.Path,
        pf.Dir,
        pf.File,
        pf.Env,
        Utils,
        System,
        State,
        State.{StateType},
        Terminal
        ]
    provides [main] to pf


mainLoop : StateType -> Task [Step StateType,Done {} ] * 
mainLoop = \ state  -> 
    _ <- Sleep.millis 50 |> Task.attempt
    termstate <- Terminal.step state |> Task.await
    if State.getAppMode termstate == Quitting then
        Task.ok (Done {})
    else 
        Task.ok (Step termstate )
peekConsole = "This is peek app : ) \n\n"

main =
    {} <- Stdout.line peekConsole |> Task.await    
    systemDataUpdated <-System.updateData (State.create  "") |> Task.await
    setupHistoryUpdated <-System.setupHistory systemDataUpdated  |> Task.await
    initTerminalStateResult <-Terminal.init setupHistoryUpdated |> Task.attempt
    
    when initTerminalStateResult is
        Ok initTerminalState ->    
            {} <-Task.loop  initTerminalState mainLoop  |> Task.await
            {} <- Tty.disableRawMode |> Task.await
            Task.ok {}
        Err _ -> Task.ok {}


    
    

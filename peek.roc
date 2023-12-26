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
        Utils,
        System,
        State,
        Commands.{quitCommand},
        State.{StateType},
        Terminal
        ]
    provides [main] to pf


mainLoop : StateType -> Task [Step StateType,Done {} ] * 
mainLoop = \ state  -> 
    _ <- Sleep.millis 100 |> Task.attempt
    termstate <- Terminal.step state |> Task.await
    if State.getCommand termstate == quitCommand then
        Task.ok (Done {})
    else
        exeState <- System.executeSystemCommand termstate |> Task.await
        cursorPosOkState <- Terminal.displayCommand exeState |> Task.await
        Task.ok (Step (State.resetActiveCommand cursorPosOkState) )

peekConsole = "This is peek app : ) \n\n"

main =
    {} <- Stdout.line peekConsole |> Task.await    
    {} <- Tty.enableRawMode |> Task.await
    initStateResult <- Terminal.init (State.create  "") |> Task.attempt
    when initStateResult is
        Ok initState ->    
            {} <-Task.loop  initState mainLoop  |> Task.await
            {} <- Tty.disableRawMode |> Task.await
            Task.ok {}
        Err _ -> Task.ok {}

    
# loop : 
#     state,     
#     (state
#     -> Task 
#         [
#             Step state,
#             Done done
#         ] err)
#     -> Task done err

#     Stdout.line "fdsdfsgsd" 
    #out <- (Cmd.output command ) |> Task.await
    
    #    dbg out
    #    Stdout.line "ok"

    
    

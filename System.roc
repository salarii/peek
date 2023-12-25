interface  System
    exposes [executeSystemCommand]
    imports [
        pf.Stdin, 
        pf.Stdout, 
        pf.Task.{ Task, await, loop },
        pf.Cmd,
        pf.Cmd.{Cmd},
        pf.Cmd.{Output },
        Utils,
        State,
        State.{StateType}
        ]


# executeSystemCommand  Str 
executeSystemCommand : Task StateType a -> Task StateType a
executeSystemCommand = \ taskState ->
    state <- taskState |> Task.await
    lstCmd = Utils.tokenize (State.getCommand  state)
    execute : Cmd -> Task StateType *
    execute = \ command -> 
        result <- Cmd.output  command |> Task.attempt
            when result is  
                Ok out ->
                    Task.ok  (State.setCommandOutput (Utils.utfToStr out.stdout) state)
                    
                Err (val,err) ->
                    Task.ok  state
    when lstCmd is 
        [] -> Task.ok state
        [name] -> 
            command =
                Cmd.new name
            execute command
        [name, .. as args] ->
            command =
                Cmd.new name
                |> Cmd.args args
            execute command
# main =
#     command =
#         Cmd.new "ls"
#         |> Cmd.args ["-all"]


#     Stdout.line "fdsdfsgsd" 


    
    

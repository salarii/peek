app "testCommands"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br"
    }
    imports [
        pf.Stdout,
        pf.File,
        pf.Path,
        pf.Dir,
        pf.Cmd,
        pf.Env,
        Utils,
        pf.Task.{ Task },
        Commands,
    ]
    provides [main] to pf

main =   

    Stdout.write  "the goal ofthis unit is to test Commands.roc"


# rudimentary  tests
expect
    when Commands.recoverConfigFromInput (Utils.tokenize "  white ") is 
        Ok config ->
            config.patterns == [ Allow "white" ] && 
            Set.isEmpty config.modifiers == Bool.true  &&  
            config.command == Search     
        Err mes -> mes == "test search pattern"
        
expect
    when Commands.recoverConfigFromInput (Utils.tokenize "  b@white ") is 
        Ok config ->
            config.patterns == [ Blacklist "white" ] && 
            Set.isEmpty config.modifiers == Bool.true  &&  
            config.command == Search     
        Err mes -> mes == "test search blacklist pattern"


expect
    when Commands.recoverConfigFromInput (Utils.tokenize "  bR@^@[6-7]white ") is 
        Ok config ->
            config.patterns == [ Regex (Blacklist "^@[6-7]white") ] && 
            Set.isEmpty config.modifiers == Bool.true  &&  
            config.command == Search     
        Err mes -> mes == "test search regex pattern"


expect
    when Commands.recoverConfigFromInput (Utils.tokenize " white Nl@ ") is 
        Ok config ->
            dest = 
                Set.empty {}
                |> Set.insert NumberLines

            config.patterns == [ Allow "white" ] && 
            config.modifiers == dest &&  
            config.command == Search     
        Err mes -> mes == "test modifier number lines"


expect
    when Commands.recoverConfigFromInput (Utils.tokenize "  fsflN@ ")  is 
        Ok config ->
            config.patterns == [ Allow "fsflN@" ]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == Search     
        Err mes -> mes == "test invalid command, empty pattern"

expect
    when Commands.recoverConfigFromInput  (Utils.tokenize "  fsflN@tt ") is 
        Ok config ->
            config.patterns == [ Allow "fsflN@tt" ]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == Search     
        Err mes -> mes == "test invalid command"

expect
    when Commands.recoverConfigFromInput  (Utils.tokenize " r@osa->@kosa white")  is 
        Ok config ->
            config.patterns == [Allow "white"]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == FromPatternToPattern (Regex (Allow "osa")) (Allow "kosa")     
        Err mes -> mes == "test from pattern to pattern"

expect
    when Commands.recoverConfigFromInput  (Utils.tokenize " s->1000@  white") is 
        Ok config ->
            config.patterns == [Allow "white"]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.limit == [FromLineToLine 1 1000]
        Err mes -> mes == "test from start to 1000 "

expect
    when Commands.recoverConfigFromInput  (Utils.tokenize " 100->e@  white")  is 
        Ok config ->
            config.patterns == [Allow "white"]  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.limit == [FromLineToLine 100 -1]   
        Err mes -> mes == "test from 100 to e "

expect
    when Commands.recoverConfigFromInput  (Utils.tokenize " ^10r@black[1-9]")    is 
        Ok config ->
            config.patterns == []  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == SearchSection {before : 10,  after : 10, pattern : (Regex (Allow "black[1-9]")) }
        Err mes -> mes == "test region "

expect
    when Commands.recoverConfigFromInput  (Utils.tokenize " <10@black") is 
        Ok config ->
            config.patterns == []  &&
            Set.isEmpty  config.modifiers == Bool.true &&  
            config.command == SearchSection {before : 10, after : 0, pattern:  (Allow "black")}
        Err mes -> mes == "test region before "


# maybe create more test in the future, at least to cover detected bugs
    app "reg"

    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf


checkMatching = \ str, reg  ->
    matchStr = \ graph, pattern ->
        when pattern is 
            Character val ->
                if val == graph then
                    Consume    
                else
                    NoMatch
            Dot ->
                Consume
    
    getFirstPat = (\  state  ->  
        when List.first state.current is
            Ok pat -> 
                { pattern : pat , state : state } 
            Err _ ->  getFirstPat  { state & current : state.regex } )
    
    Str.graphemes  str
    |> List.walk  { regex : reg, current : reg, matched : "", result : Bool.false }  ( \ state, graph ->
        if state.result == Bool.true then
            state
        else
            matchThis = getFirstPat state
            updatedState = matchThis.state

            when matchStr graph  matchThis.pattern is 
                Consume ->
                    if List.isEmpty updatedState.current == Bool.true then
                        { updatedState & matched : Str.concat updatedState.matched graph, result : Bool.true  }
                    else 
                        { updatedState & matched : Str.concat updatedState.matched graph, current : List.dropFirst  updatedState.current }
                NoMatch ->   
                        { updatedState & matched : "", current : updatedState.regex } )


main =  
    res = checkMatching "dsads"  [Dot] 
    
    Stdout.line "adfasfsa"
    
    

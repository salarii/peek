app "command"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br"
    }
    #exposes []
    imports [
        pf.Stdout,
        pf.Stdin,
        Regex,
    ]
    provides [main] to pf

# first  cut
Pattern : [ Regex  Str, Positive Str, Negative Str  ]


Command : [
    NumberLines,
    SearchSection U32 U32 Pattern,
    FromLineToLine  U32 U32,
    FromPatternToPattern Pattern Pattern,
]

populationByCity =
    Dict.empty {}
    |> Dict.insert "^[Nn][Ll]"  "listlinesdummy"
    |> Dict.insert "@(.+)->@(.+)" "patterntopattern"
    |> Dict.insert "^(\\d+)->(\\d+)@$" "from  line to line"
    |> Dict.insert "^\\^(\\d+)" "section"
    |> Dict.insert "^@$" "negative"

simplifiedSyntax = 
    Dict.empty {}

recoverCommandFromInput : Str -> List  Command
recoverCommandFromInput = \commandStr ->
    [NumberLines]



main = 
    #Stdout.write  clearScreenPat |> Task.await
    Stdout.write  "kapusta"





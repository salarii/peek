    
main =


    chainRange =
        []
        |> List.append  (createToken  CaptureOpen  Once Bool.false)
        |> List.append  (createToken (Sequence [(createToken  Dot  Once Bool.false)] )  Once Bool.false)
        |> List.append  (createToken  CaptureClose  Once Bool.false)
        |> List.append  (createToken  ( Character '-' )  Once Bool.false )
        |> List.append  (createToken  CaptureOpen  Once Bool.false)
        |> List.append  (createToken (Sequence [(createToken  Dot  Once Bool.false)] )  Once Bool.false)
        |> List.append  (createToken  CaptureClose  Once Bool.false)
        
    chainDot = 
        []
        |> List.append  (createToken  CaptureOpen  Once Bool.false)
        |> List.append  (createToken (Sequence [(createToken  Dot  Once Bool.false)] )  Once Bool.false)
        |> List.append  (createToken  CaptureClose  Once Bool.false)
        
    chainMain = 
        []
        |> List.append  (createToken  CaptureOpen  Once Bool.false)
        |> List.append  (createToken (Sequence chainRange )  Once Bool.false)
        |> List.append  (createToken  CaptureClose  Once Bool.false)
        |> List.append  (createToken  Separator  Once Bool.false)
        |> List.append  (createToken  CaptureOpen  Once Bool.false)
        |> List.append  (createToken (Sequence chainDot )  Once Bool.false)
        |> List.append  (createToken  CaptureClose  Once Bool.false)
    
    pat  =
        []
        |> List.append  (createToken  ( Character '[' )  Once Bool.false )
        |> List.append  (createToken  CaptureOpen  Once Bool.false)
        |> List.append  (createToken (Sequence  chainMain)  Once Bool.false) 
        |> List.append  (createToken  CaptureClose  Once Bool.false)
        |> List.append  (createToken  ( Character ']' )  Once Bool.false )
     
     
    #dbg  printMe   pat
    #res = checkMatching (Str.toUtf8  "[a-g]" )  pat
    #res = checkMatching (Str.toUtf8  "a" )  [(createToken  ( Character 'a' )  Once Bool.false )]
    
    #res =      
    #    when parseStr "asfgsdgaaccaaaa" "aa(bb|cc)aa" is 
    #        Ok parsed ->
    #            parsed.result == Bool.true
    #        Err mes -> mes == "test separator matching" 
    #dbg  res

   # Stdout.line "outStr"

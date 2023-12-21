interface Utils
    exposes [asciiArrayToNumber, tokenize, utfToStr]
    imports []

#NumType : I32
#conversionFun : (Str -> Result NumType [InvalidNumStr])
#conversionFun = Str.toI32


#asciiArrayToNumberr : List U8 -> NumType
#asciiArrayToNumberr = \ lst -> 
#    Str.fromUtf8 lst
#    |> Result.withDefault "0"
#    |> conversionFun
#    |> Result.withDefault 0



asciiArrayToNumber : List U8, (Str -> Result (Num a) [InvalidNumStr]) -> Num a
asciiArrayToNumber = \ lst, convert -> 
    Str.fromUtf8 lst
    |> Result.withDefault "0"
    |> convert
    |> Result.withDefault 0

tokenize  = \ str  -> 
    Str.split str  " "
    |> List.dropIf  Str.isEmpty
    
utfToStr : List U8  -> Str
utfToStr = \ lst ->
    when  Str.fromUtf8 lst  is 
        Ok str -> str
        Err _ -> ""

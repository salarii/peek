interface Utils
    exposes [asciiArrayToNumber]
    imports []

NumType : I32
conversionFun : (Str -> Result NumType [InvalidNumStr])
conversionFun = Str.toI32


asciiArrayToNumber : List U8 -> NumType
asciiArrayToNumber = \ lst -> 
    Str.fromUtf8 lst
    |> Result.withDefault "0"
    |> conversionFun
    |> Result.withDefault 0
interface Utils
    exposes [asciiArrayToNumber]
    imports []

NumType : I32
conversionFun : (Str -> Result NumType [InvalidNumStr])
conversionFun = Str.toI32


asciiArrayToNumberr : List U8 -> NumType
asciiArrayToNumberr = \ lst -> 
    Str.fromUtf8 lst
    |> Result.withDefault "0"
    |> conversionFun
    |> Result.withDefault 0



asciiArrayToNumber : List U8, (Str -> Result (Num a) [InvalidNumStr]) -> Num a
asciiArrayToNumber = \ lst, convert -> 
    Str.fromUtf8 lst
    |> Result.withDefault "0"
    |> convert
    |> Result.withDefault 0
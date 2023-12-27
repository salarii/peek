interface Utils
    exposes [asciiArrayToNumber, tokenize, utfToStr, modifyLastInList, withColor]
    imports []

withColor : Str, [Red, Green, Blue]-> Str
withColor = \str, color ->
    when color is 
        Red -> "\u(001b)[31m\(str)\u(001b)[0m"
        Green -> "\u(001b)[32m\(str)\u(001b)[0m"
        Blue -> "\u(001b)[34m\(str)\u(001b)[0m"



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


modifyLastInList : List  a, a -> List  a
modifyLastInList = \ lst, elem ->
    List.dropLast lst 1
    |> List.append elem

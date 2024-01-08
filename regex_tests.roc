app "testRegex"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br"
    }
    imports [
        pf.Stdout,
        Regex,
    ]
    provides [main] to pf

main =   
    kk =
        when Regex.parseStr "white" "(u?w(h)i(t)e)" is 
            Ok parsed ->
                dbg parsed.captured 
                parsed.matchFound == Bool.false
            Err mes -> mes == "test except of matching" 


    Stdout.write  "the goal ofthis unit is to test Regex.roc"


 #   chainRange =
 #       []
 #       |> List.append  (createToken  CaptureOpen  Once Bool.false)
 #       |> List.append  (createToken (Sequence [(createToken  Dot  Once Bool.false)] )  Once Bool.false)
 #       |> List.append  (createToken  CaptureClose  Once Bool.false)
 #       |> List.append  (createToken  ( Character '-' )  Once Bool.false )
 #       |> List.append  (createToken  CaptureOpen  Once Bool.false)
 #       |> List.append  (createToken (Sequence [(createToken  Dot  Once Bool.false)] )  Once Bool.false)
 #       |> List.append  (createToken  CaptureClose  Once Bool.false)
        
 #   chainDot = 
 #       []
 #       |> List.append  (createToken  CaptureOpen  Once Bool.false)
 #       |> List.append  (createToken (Sequence [(createToken  Dot  Once Bool.false)] )  Once Bool.false)
 #       |> List.append  (createToken  CaptureClose  Once Bool.false)
        
  #  chainMain = 
  #      []
   #     |> List.append  (createToken  CaptureOpen  Once Bool.false)
   #     |> List.append  (createToken (Sequence chainRange )  Once Bool.false)
#        |> List.append  (createToken  CaptureClose  Once Bool.false)
#        |> List.append  (createToken  Separator  Once Bool.false)
#        |> List.append  (createToken  CaptureOpen  Once Bool.false)
#        |> List.append  (createToken (Sequence chainDot )  Once Bool.false)
#        |> List.append  (createToken  CaptureClose  Once Bool.false)
    
 #   pat  =
  #      []
  #      |> List.append  (createToken  ( Character '[' )  Once Bool.false )
  #      |> List.append  (createToken  CaptureOpen  Once Bool.false)
  #      |> List.append  (createToken (Sequence  chainMain)  Once Bool.false) 
  #      |> List.append  (createToken  CaptureClose  Once Bool.false)
  #      |> List.append  (createToken  ( Character ']' )  Once Bool.false )
     
     

#################### succeed/failed tests ####################
    
# rudimentary tests
expect
    when Regex.parseStr "a" "a" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test characters matching"
               
expect
    when Regex.parseStr "b" "a" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test characters matching"

expect
    when Regex.parseStr "aaaabcc" "abc" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test characters matching"  
        
expect
    when Regex.parseStr "aaaabbcabcadsa" "abc" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test characters matching"  
        
expect
    when Regex.parseStr "aaaabcdfa" "abcdef" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test characters matching"
        
expect
    when Regex.parseStr "aghdsad" "." is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test dot matching"
        
expect
    when Regex.parseStr "agh" "...." is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test dot matching"
        
expect
    when Regex.parseStr "abcd323sfsddgf" "\\d\\d\\d" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test digit matching"
        
expect
    when Regex.parseStr "abcd32sfsddgf" "\\d\\d\\d" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test digit matching"
        
expect
    when Regex.parseStr "24423abd45236436" "\\D\\D\\D" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test digit matching"
        
expect
    when Regex.parseStr "24423ab3d45236436" "\\D\\D\\D" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test digit matching"
        
expect
    when Regex.parseStr "24423...45236436" "\\.\\.\\." is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test period matching"
        
expect
    when Regex.parseStr "24423..3d.45236436" "\\.\\.\\." is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test period matching"
        
expect
    when Regex.parseStr "2A_d" "\\w\\w\\w\\w" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test alphanumeric matching"
        
expect
    when Regex.parseStr "2A_]{d" "\\w\\w\\w\\w" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test alphanumeric matching"
            
expect
    when Regex.parseStr "#@$a%^&*" "\\W\\W\\W\\W" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test non-alphanumeric matching"
        
expect
    when Regex.parseStr "#@$a%^_&*" "\\W\\W\\W\\W" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test non-alphanumeric matching"    
        
expect
    str = "  dd  \n\rdsda   d "
    when Regex.parseStr str "\\s\\s\\s\\s" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test whitespaces matching"
        
expect
    str = "  dd  \ndsda   d "
    when Regex.parseStr str "\\s\\s\\s\\s" is 
    Ok parsed ->
        parsed.matchFound == Bool.false
    Err mes -> mes == "test whitespaces matching"
        
expect
    str = "  dd  \n\rdsda   d "
    when Regex.parseStr str "\\S\\S\\S\\S" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test non-whitespaces matching"
        
expect
    str = "  dd  \ndsd\ra   d "
    when Regex.parseStr str "\\S\\S\\S\\S" is 
    Ok parsed ->
        parsed.matchFound == Bool.false
    Err mes -> mes == "test non-whitespaces matching"  

expect
    when Regex.parseStr "abcd" "ab?c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test optional character matching"

expect
    when Regex.parseStr "acd" "ab?c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test optional character matching"

expect        
    when Regex.parseStr "abbcd" "ab?c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test optional character matching"

expect
    when Regex.parseStr "abcd" "a(bc)?d" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test optional characters matching"
 
 expect
    when Regex.parseStr "ad" "a(bc)?d" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test optional characters matching"

expect 
    when Regex.parseStr "abd" "a(bc)?d" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test optional characters matching"
      
expect  
    when Regex.parseStr "abcbcd" "a(bc)?d" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test optional characters matching"
        
expect        
    when Regex.parseStr "abbbcd" "ab*c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test zero or more matching"

expect        
    when Regex.parseStr "acd" "ab*c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test zero or more matching"

expect
    when Regex.parseStr "abcbcbcdfbcdbgc" "a(bc)*d" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test zero or more matching"  
        
expect
    when Regex.parseStr "abcfdadbgc" "a(bc)*d" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test zero or more matching"  
        
expect
    when Regex.parseStr "abxcfbxcdbgc" "a(bc)*d" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test zero or more matching" 
        
expect        
    when Regex.parseStr "abbbcd" "ab+c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test at least one matching"

expect        
    when Regex.parseStr "acd" "ab+c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test at least one matching"

expect
    when Regex.parseStr "abcbcbcdfbcdbgc" "a(bc)+d" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test at least one matching"  
        
expect
    when Regex.parseStr "abcfdadbgc" "a(bc)+d" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test at least one matching"  
        
expect
    when Regex.parseStr "abxcfbxcdbgc" "a(bc)+d" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test at least one matching"  
        
expect        
    when Regex.parseStr "abbbcd" "ab{3}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test repetition matching" 
        
expect        
    when Regex.parseStr "abbbbbbbbbbbcd" "ab{11}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test repetition matching" 
        
expect        
    when Regex.parseStr "abbbbcd" "ab{3}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition matching" 

expect        
    when Regex.parseStr "abbcd" "ab{3}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition matching" 
        
expect        
    when Regex.parseStr "abbbbbbcd" "a(bb){3}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test repetition matching" 
        
expect        
    when Regex.parseStr "abbbbbbbcd" "a(bb){3}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition matching" 
        
expect        
    when Regex.parseStr "abbcd" "a(bb){3}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition matching"
        
expect        
    when Regex.parseStr "abcd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when Regex.parseStr "abbcd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when Regex.parseStr "acd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition range matching"  
        
        
expect        
    when Regex.parseStr "abbbcd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when Regex.parseStr "abbbbbbbbbbcd" "ab{10,11}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test repetition range matching" 
        
expect        
    when Regex.parseStr "abbbbbbbbbcd" "ab{10,11}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition range matching"
        
expect        
    when Regex.parseStr "abcd" "ab{1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when Regex.parseStr "abbcd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when Regex.parseStr "abbbbcd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when Regex.parseStr "abbbcd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition range matching"  
        
expect        
    when Regex.parseStr "abbbbbbcd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition range matching"  
         
expect        
    when Regex.parseStr "acd" "a(bb){1,2}c" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test repetition range matching"  
         
expect        
    when Regex.parseStr "dad" "d[a-g]d" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test one of matching"  
         
expect        
    when Regex.parseStr "abhd" "ab[a-g]d" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test one of matching"  
expect        
    when Regex.parseStr "a6a" "a[a-g1-7]a" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test one of matching" 
        
expect        
    when Regex.parseStr "a8a" "a[a-g1-7]a" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test one of matching" 
        
expect        
    when Regex.parseStr "zcz" "z[abcd]z" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test one of matching" 
        
expect        
    when Regex.parseStr "zfz" "z[abcd]z" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test one of matching" 
        
expect        
    when Regex.parseStr "z*z" "z[\\*\\?\\{]z" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test one of matching" 
        
expect        
    when Regex.parseStr "z^z" "z[\\*\\?\\{]z" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test one of matching" 
        
expect        
    when Regex.parseStr "dad" "d[^a-g]d" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test except of matching"      
        
expect        
    when Regex.parseStr "abhd" "ab[^a-g]d" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test except of matching"  
expect        
    when Regex.parseStr "a6a" "a[^a-g1-7]a" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test except of matching" 
        
expect        
    when Regex.parseStr "a8a" "a[^a-g1-7]a" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test except of matching" 
        
expect        
    when Regex.parseStr "zcz" "z[^abcd]z" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test except of matching" 
        
expect        
    when Regex.parseStr "zfz" "z[^abcd]z" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test except of matching" 
        
expect        
    when Regex.parseStr "z*z" "z[^\\*\\?\\{]z" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test except of matching" 
        
expect        
    when Regex.parseStr "z^z" "z[^\\*\\?\\{]z" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test one of matching" 
        
expect        
    when Regex.parseStr "abcdefgh" "^abc" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test start end matching"     
        
expect        
    when Regex.parseStr "ffggabcdefgh" "^abc" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test start end matching"  
        
expect        
    when Regex.parseStr "aabc" "abc$" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test start end matching"     
        
        
expect        
    when Regex.parseStr "ffggabcde" "abc$" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test start end matching"   
        

expect        
    when Regex.parseStr "abc" "^abc$" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test start end matching"  
        
expect        
    when Regex.parseStr "aabc" "^abc$" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test start end matching"     
        
        
expect        
    when Regex.parseStr "abcde" "^abc$" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test start end matching"
        
expect        
    when Regex.parseStr "ffggabcdeabc" "^abc|abc$" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test start end matching"  
          
expect        
    when Regex.parseStr "ffggaaxyzbc" "abc|axyz|jjjj" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test separator matching"  
          
expect        
    when Regex.parseStr "ffggjjjjzbc" "abc|axyz|jjjj" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test separator matching"  
          
expect        
    when Regex.parseStr "ffggjjjzbc" "abc|axyz|jjjj" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test separator matching"  
          
expect        
    when Regex.parseStr "aaaccaaaa" "aa(bb|cc)aa" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "test separator matching"  
          
expect        
    when Regex.parseStr "aaacccaaaa" "aa(bb|cc)aa" is 
        Ok parsed ->
            parsed.matchFound == Bool.false
        Err mes -> mes == "test separator matching"  
          
# more  complex  test

#for now I consider research phase as done , I should move to stabilisation phase now
#but I will postpone this a bit. 
#so in near future, and conduct fair bit of more comprehensive testing   

# bugs  detected 
expect        
    when Regex.parseStr "c@pattern" "^[cC]@(.+)" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "bug only at front"  

expect        
    when Regex.parseStr "nl@" "^[Nn][Ll]@$" is 
        Ok parsed ->
            parsed.matchFound == Bool.true
        Err mes -> mes == "bug two only sections"  

expect
when Regex.parseStr "white" "((u)?white)" is 
            Ok parsed ->
                parsed.matchFound == Bool.true
            Err mes -> mes == "bug optional on nested sequence" 
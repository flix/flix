namespace Pkger/Parser {
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // pure                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def pure1(): Bool =
        ('a' :: Nil) |> pure(0) == Some(('a' :: Nil, 0))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // char                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def char1(): Bool =
        Nil |> char() == None
    
    @test
    def char2(): Bool =
        ('a' :: Nil) |> char() == Some((Nil, 'a'))
    
    @test
    def char3(): Bool =
        ('a' :: 'b' :: Nil) |> char() == Some(('b' :: Nil, 'a'))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // filter                                                                                    //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def filter1(): Bool =
        ('0' :: Nil) |> char().filter(Char.isDigit) == Some((Nil, '0'))
    
    @test
    def filter2(): Bool =
       ('a' :: Nil) |> char().filter(Char.isDigit) == None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // tag                                                                                       //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def tag1(): Bool =
        ('a' :: Nil) |> tag('a') == Some((Nil, 'a'))
    
    @test
    def tag2(): Bool =
        ('b' :: Nil) |> tag('a') == None
    
    @test
    def tag3(): Bool =
        ('a':: 'b' :: Nil) |> tag('a') == Some(('b' :: Nil, 'a'))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // map                                                                                       //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def map1(): Bool =
        ('a' :: Nil) |> char().map(c -> c == 'a') == Some((Nil, true))
    
    @test
    def map2(): Bool =
        ('a' :: Nil) |> char().map(c -> if (c == 'a') {
            'b'
        } else {
            'y'
        })
        ==
        Some((Nil, 'b'))
    
    @test
    def map3(): Bool =
        String.toList("abc") |> separatedAny(char(), tag('b')).map(List.toString)
        ==
        Some((Nil, "ac"))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // and                                                                                       //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def and1(): Bool =
        ('a' :: 'b' :: Nil) |> (tag('a') `and` tag('b')) == Some((Nil, ('a', 'b')))
    
    @test
    def and2(): Bool =
        ('a' :: 'a' :: Nil) |> (tag('a') `and` tag('b')) == None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // or                                                                                        //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def or1(): Bool =
        ('a' :: 'b' :: Nil) |> (tag('a') `or` tag('b')) == Some(('b' :: Nil, 'a'))
    
    @test
    def or2(): Bool =
        ('b' :: 'a' :: Nil) |> (tag('a') `or` tag('b')) == Some(('a' :: Nil, 'b'))
    
    @test
    def or3(): Bool =
        ('z' :: 'z' :: Nil) |> (tag('a') `or` tag('b')) == None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // then                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def then1(): Bool =
        let parser = (char() `and` char())
            .then(match (lhs, rhs) -> tag('+')
            .map(v -> lhs :: v :: rhs :: Nil));
        String.toList("12+") |> parser == Some((Nil, '1' :: '+' :: '2' :: Nil))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // not                                                                                       //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def not1(): Bool =
        ('a' :: Nil) |> not(tag('a')) == None
    
    @test
    def not2(): Bool =
        ('b' :: Nil) |> not(tag('a')) == Some(('b' :: Nil, ()))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // optional                                                                                  //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def optional1(): Bool =
        ('a' :: Nil) |> optional(tag('a')) == Some((Nil, Some('a')))
    
    @test
    def optional2(): Bool =
        ('b' :: Nil) |> optional(tag('a')) == Some(('b' :: Nil, None))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // skip                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def skip1(): Bool =
        ('a' :: Nil) |> skip(char()) == Some((Nil, ()))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // skipLeft                                                                                  //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def skipLeft1(): Bool =
        String.toList("abc") |> (char() `skipLeft` char())
        ==
        Some(('c' :: Nil, 'b'))
    
    @test
    def skipLeft2(): Bool =
        String.toList("abc") |> (tag('b') `skipLeft` char()) == None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // skipRight                                                                                 //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    
    @test
    def skipRight1(): Bool =
        String.toList("abc") |> (char() `skipRight` char())
        ==
        Some(('c' :: Nil, 'a'))
    
    @test
    def skipRight2(): Bool =
        String.toList("abc") |> (char() `skipRight` tag('a')) == None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // take                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def take1(): Bool =
        String.toList("abc") |> char().take(2) == Some(('c' :: Nil, 'a' :: 'b' :: Nil))
    
    @test
    def take2(): Bool =
        ('a' :: Nil) |> char().take(2) == None
    
    @test
    def take3(): Bool =
        ('a' :: Nil) |> char().take(0) == Some(('a' :: Nil, Nil))
    
    @test
    def take4(): Bool =
        ('a' :: Nil) |> char().take(-1) == Some(('a' :: Nil, Nil))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // separatedTake                                                                             //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def separatedTake1(): Bool =
        String.toList("a.b.c") |> separatedTake(char(), tag('.'), 2)
        ==
        Some(('.' :: 'c' :: Nil, 'a' :: 'b' :: Nil))
    
    @test
    def separatedTake2(): Bool =
        ('a' :: '.' :: Nil) |> separatedTake(char(), tag('.'), 2)
        ==
        None
    
    @test
    def separatedTake3(): Bool =
        ('.' :: 'a' :: Nil) |> separatedTake(char(), tag('.'), 0)
        ==
        Some(('.' :: 'a' :: Nil, Nil))
    
    @test
    def separatedTake4(): Bool =
        ('.' :: 'a' :: Nil) |> separatedTake(char(), tag('.'), -1)
        ==
        Some(('.' :: 'a' :: Nil, Nil))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // tailSeparatedTake                                                                         //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def tailSeparatedTake1(): Bool =
        String.toList("a.b.c") |> tailSeparatedTake(char(), tag('.'), 2)
        ==
        Some(('c' :: Nil, 'a' :: 'b' :: Nil))
    
    @test
    def tailSeparatedTake2(): Bool =
        ('a' :: '.' :: Nil) |> tailSeparatedTake(char(), tag('.'), 2) == None
    
    @test
    def tailSeparatedTake3(): Bool =
        ('.' :: 'a' :: Nil) |> tailSeparatedTake(char(), tag('.'), 0) == Some(('a' :: Nil, Nil))
    
    @test
    def tailSeparatedTake4(): Bool =
        ('.' :: 'a' :: Nil) |> tailSeparatedTake(char(), tag('.'), -1) == Some(('a' :: Nil, Nil))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // takeAny                                                                                   //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def takeAny1(): Bool =
        Nil |> takeAny(tag('a')) == Some((Nil, Nil))
    
    @test
    def takeAny2(): Bool =
        ('a' :: Nil) |> takeAny(tag('a')) == Some((Nil, 'a' :: Nil))
    
    @test
    def takeAny3(): Bool = 
        ('a' :: 'b' :: Nil) |> takeAny(char().filter(Char.isLetter))
        ==
        Some((Nil, 'a' :: 'b' :: Nil))
    
    @test
    def takeAny4(): Bool = 
        ('a' :: 'b' :: '-' :: Nil) |> takeAny(char().filter(Char.isLetter))
        ==
        Some(('-' :: Nil, 'a' :: 'b' :: Nil))
    
    @test
    def takeAny5(): Bool = 
    
        ('a' :: 'b' :: '-' :: 'c' :: Nil) |> takeAny(char().filter(Char.isLetter))
        ==
        Some(('-' :: 'c' :: Nil, 'a' :: 'b' :: Nil))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // takeMany                                                                                  //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def takeMany1(): Bool = 
        ('a' :: 'b' :: Nil) |> takeMany(char().filter(Char.isLetter))
        ==
        Some((Nil, 'a' :: 'b' :: Nil))
    
    @test
    def takeMany2(): Bool = 
        ('a' :: 'b' :: '-' :: Nil) |> takeMany(char().filter(Char.isLetter))
        ==
        Some(('-' :: Nil, 'a' :: 'b' :: Nil))
    
    @test
    def takeMany3(): Bool = 
    
        ('a' :: 'b' :: '-' :: 'c' :: Nil) |> takeMany(char().filter(Char.isLetter))
        ==
        Some(('-' :: 'c' :: Nil, 'a' :: 'b' :: Nil))
    
    @test
    def takeMany4(): Bool = 
    
        ('_' :: 'a' :: 'b' :: 'c' :: Nil) |> takeMany(char().filter(Char.isLetter))
        ==
        None
    
    @test
    def takeMany5(): Bool = 
        Nil |> takeMany(char().filter(Char.isLetter)) == None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // separatedAny                                                                              //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def separatedAny1(): Bool =
        String.toList("ababa") |> separatedAny(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
    
    @test
    def separatedAny2(): Bool =
        String.toList("abab") |> separatedAny(tag('a'), tag('b'))
        ==
        Some(('b' :: Nil, 'a' :: 'a' :: Nil))
    
    @test
    def separatedAny3(): Bool =
        ('a' :: Nil) |> separatedAny(tag('a'), tag('b')) == Some((Nil, 'a' :: Nil))
    
    @test
    def separatedAny4(): Bool =
        ('b' :: Nil) |> separatedAny(tag('a'), tag('b')) == Some(('b' :: Nil, Nil))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // separatedMany                                                                             //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def separatedMany1(): Bool =
        String.toList("ababa") |> separatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
    
    @test
    def separatedMany2(): Bool =
        String.toList("abab") |> separatedMany(tag('a'), tag('b'))
        ==
        Some(('b' :: Nil, 'a' :: 'a' :: Nil))
    @test
    def separatedMany3(): Bool =
        ('a' :: Nil) |> separatedMany(tag('a'), tag('b')) == Some((Nil, 'a' :: Nil))
    @test
    def separatedMany4(): Bool =
        ('b' :: Nil) |> separatedMany(tag('a'), tag('b')) == None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // tailSeparatedAny                                                                          //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def tailSeparatedAny1(): Bool =
        String.toList("ababa") |> tailSeparatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
    
    @test
    def tailSeparatedAny2(): Bool =
        String.toList("abab") |> tailSeparatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: Nil))
    
    @test
    def tailSeparatedAny3(): Bool =
        ('a' :: Nil) |> tailSeparatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: Nil))
    
    @test
    def tailSeparatedAny4(): Bool =
        ('b' :: Nil) |> tailSeparatedMany(tag('a'), tag('b'))
        ==
        None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // tailSeparatedMany                                                                         //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def tailSeparatedMany1(): Bool =
        String.toList("ababa") |> tailSeparatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
    
    @test
    def tailSeparatedMany2(): Bool =
        String.toList("abab") |> tailSeparatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: Nil))
    
    @test
    def tailSeparatedMany3(): Bool =
        ('a' :: Nil) |> tailSeparatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: Nil))
    
    @test
    def tailSeparatedMany4(): Bool =
        ('b' :: Nil) |> tailSeparatedMany(tag('a'), tag('b'))
        ==
        None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // delimit                                                                                   //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def delimit1(): Bool =
        String.toList("[abz]") |>
            delimit(tag('['), takeAny(char().filter(Char.isLetter)), tag(']'))
        ==
        Some((Nil, 'a' :: 'b' :: 'z' :: Nil))
    
    @test
    def delimit2(): Bool =
        String.toList("[a.bz]") |>
            delimit(tag('['), takeAny(char().filter(Char.isLetter)), tag(']'))
        ==
        None
    
    @test
    def delimit3(): Bool =
        ('[' :: ']' :: Nil) |>
            delimit(tag('['), takeAny(char().filter(Char.isLetter)), tag(']'))
        ==
        Some((Nil, Nil))
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // fold                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def fold1(): Bool =
        String.toList("abc") |> fold(char(), Nil, l -> c -> c :: l)
        ==
        Some((Nil, 'c' :: 'b' :: 'a' :: Nil))
}

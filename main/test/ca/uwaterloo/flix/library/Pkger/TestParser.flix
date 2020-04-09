namespace Parser/Test {

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // pure                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def pure1(): Bool =
        use Parser.pure;
        String.toList("a") |> pure(0) == Some(('a' :: Nil, 0))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // char                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////
       
    @test
    def char1(): Bool =
        use Parser.char;
        Nil |> char() == None

    @test
    def char2(): Bool =
        use Parser.char;
        String.toList("a") |> char() == Some((Nil, 'a'))

    @test
    def char3(): Bool =
        use Parser.char;
        String.toList("ab") |> char() == Some(('b' :: Nil, 'a'))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // filter                                                                                    //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def filter1(): Bool =
        use Parser.{filter, char};
        String.toList("0") |> char().filter(Char.isDigit) == Some((Nil, '0'))
       
    @test
    def filter2(): Bool =
        use Parser.{filter, char};
        String.toList("a") |> char().filter(Char.isDigit) == None

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // tag                                                                                       //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def tag1(): Bool =
        use Parser.tag;
        String.toList("a") |> tag('a') == Some((Nil, 'a'))
       
    @test
    def tag2(): Bool =
        use Parser.tag;
        String.toList("b") |> tag('a') == None
       
    @test
    def tag3(): Bool =
        use Parser.tag;
        String.toList("ab") |> tag('a') == Some(('b' :: Nil, 'a'))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // map                                                                                       //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def map1(): Bool =
        use Parser.{map, char};
        String.toList("a") |> char().map(c -> c == 'a') == Some((Nil, true))
       
    @test
    def map2(): Bool =
        use Parser.{map, char};
        String.toList("a") |> char().map(c -> if (c == 'a') {
            'b'
        } else {
            'y'
        })
        ==
        Some((Nil, 'b'))
       
    @test
    def map3(): Bool =
        use Parser.{map, char, seperatedAny, tag};
        String.toList("abc") |> seperatedAny(char(), tag('b')).map(List.toString)
        ==
        Some((Nil, "ac"))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // and                                                                                       //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def and1(): Bool =
        use Parser.{tag, and};
        String.toList("ab") |> (tag('a') `and` tag('b')) == Some((Nil, ('a', 'b')))
       
    @test
    def and2(): Bool =
        use Parser.{tag, and};
        String.toList("aa") |> (tag('a') `and` tag('b')) == None

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // or                                                                                        //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
    @test
    def or1(): Bool =
        use Parser.{tag, or};
        String.toList("ab") |> (tag('a') `or` tag('b')) == Some(('b' :: Nil, 'a'))
       
    @test
    def or2(): Bool =
        use Parser.{tag, or};
        String.toList("ba") |> (tag('a') `or` tag('b')) == Some(('a' :: Nil, 'b'))
       
    @test
    def or3(): Bool =
        use Parser.{tag, or};
        String.toList("zz") |> (tag('a') `or` tag('b')) == None

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // then                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def then1(): Bool =
        use Parser.{then, char, and, tag, map};
        let parser = (char() `and` char())
            .then(match (lhs, rhs) -> tag('+')
            .map(v -> lhs :: v :: rhs :: Nil));
        String.toList("12+") |> parser == Some((Nil, '1' :: '+' :: '2' :: Nil))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // not                                                                                       //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def not1(): Bool =
        use Parser.{not, tag};
        String.toList("a") |> not(tag('a')) == None
       
    @test
    def not2(): Bool =
        use Parser.{not, tag};
        String.toList("b") |> not(tag('a')) == Some(('b' :: Nil, ()))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // optional                                                                                  //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def optional1(): Bool =
        use Parser.{optional, tag};
        String.toList("a") |> optional(tag('a')) == Some((Nil, Some('a')))
       
    @test
    def optional2(): Bool =
        use Parser.{optional, tag};
        String.toList("b") |> optional(tag('a')) == Some(('b' :: Nil, None))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // skip                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def skip1(): Bool =
        use Parser.{skip, char};
        String.toList("a") |> skip(char()) == Some((Nil, ()))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // skipLeft                                                                                  //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def skipLeft1(): Bool =
        use Parser.{char, skipLeft};
        String.toList("abc") |> (char() `skipLeft` char())
        ==
        Some(('c' :: Nil, 'b'))
       
    @test
    def skipLeft2(): Bool =
        use Parser.{char, tag, skipLeft};
        String.toList("abc") |> (tag('b') `skipLeft` char()) == None

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // skipRight                                                                                 //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    
    @test
    def skipRight1(): Bool =
        use Parser.{char, skipRight};
        String.toList("abc") |> (char() `skipRight` char())
        ==
        Some(('c' :: Nil, 'a'))
       
    @test
    def skipRight2(): Bool =
        use Parser.{char, skipRight, tag};
        String.toList("abc") |> (char() `skipRight` tag('a')) == None

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // take                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def take1(): Bool =
        use Parser.{char, take};
        String.toList("abc") |> char().take(2) == Some(('c' :: Nil, 'a' :: 'b' :: Nil))
       
    @test
    def take2(): Bool =
        use Parser.{char, take};
        String.toList("a") |> char().take(2) == None
       
    @test
    def take3(): Bool =
        use Parser.{char, take};
        String.toList("a") |> char().take(0) == Some(('a' :: Nil, Nil))

    @test
    def take4(): Bool =
        use Parser.{char, take};
        String.toList("a") |> char().take(-1) == Some(('a' :: Nil, Nil))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // seperatedTake                                                                             //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def seperatedTake1(): Bool =
        use Parser.{seperatedTake, char, tag};
        String.toList("a.b.c") |> seperatedTake(char(), tag('.'), 2)
        ==
        Some(('.' :: 'c' :: Nil, 'a' :: 'b' :: Nil))
       
    @test
    def seperatedTake2(): Bool =
        use Parser.{seperatedTake, char, tag};
        String.toList("a.") |> seperatedTake(char(), tag('.'), 2)
        ==
        None
       
    @test
    def seperatedTake3(): Bool =
        use Parser.{seperatedTake, char, tag};
        String.toList(".a") |> seperatedTake(char(), tag('.'), 0)
        ==
        Some(('.' :: 'a' :: Nil, Nil))
       
    @test
    def seperatedTake4(): Bool =
        use Parser.{seperatedTake, char, tag};
        String.toList(".a") |> seperatedTake(char(), tag('.'), -1)
        ==
        Some(('.' :: 'a' :: Nil, Nil))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // tailSeperatedTake                                                                         //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def tailSeperatedTake1(): Bool =
        use Parser.{tailSeperatedTake, char, tag};
        String.toList("a.b.c") |> tailSeperatedTake(char(), tag('.'), 2)
        ==
        Some(('c' :: Nil, 'a' :: 'b' :: Nil))
       
    @test
    def tailSeperatedTake2(): Bool =
        use Parser.{tailSeperatedTake, char, tag};
        String.toList("a.") |> tailSeperatedTake(char(), tag('.'), 2) == None
       
    @test
    def tailSeperatedTake3(): Bool =
        use Parser.{tailSeperatedTake, char, tag};
        String.toList(".a") |> tailSeperatedTake(char(), tag('.'), 0) == Some(('a' :: Nil, Nil))
       
    @test
    def tailSeperatedTake4(): Bool =
        use Parser.{tailSeperatedTake, char, tag};
        String.toList(".a") |> tailSeperatedTake(char(), tag('.'), -1) == Some(('a' :: Nil, Nil))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // takeAny                                                                                   //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def takeAny1(): Bool =
        use Parser.{takeAny, tag};
        Nil |> takeAny(tag('a')) == Some((Nil, Nil))
    
    @test
    def takeAny2(): Bool =
        use Parser.{takeAny, tag};
        ('a' :: Nil) |> takeAny(tag('a')) == Some((Nil, 'a' :: Nil))

    @test
    def takeAny3(): Bool = 
        use Parser.{takeAny, char, filter};
        ('a' :: 'b' :: Nil) |> takeAny(char().filter(Char.isLetter))
        ==
        Some((Nil, 'a' :: 'b' :: Nil))

    @test
    def takeAny4(): Bool = 
        use Parser.{takeAny, char, filter};

        ('a' :: 'b' :: '-' :: Nil) |> takeAny(char().filter(Char.isLetter))
        ==
        Some(('-' :: Nil, 'a' :: 'b' :: Nil))

    @test
    def takeAny5(): Bool = 
        use Parser.{takeAny, char, filter};

        ('a' :: 'b' :: '-' :: 'c' :: Nil) |> takeAny(char().filter(Char.isLetter))
        ==
        Some(('-' :: 'c' :: Nil, 'a' :: 'b' :: Nil))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // takeMany                                                                                  //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def takeMany1(): Bool = 
        use Parser.{takeMany, char, filter};
        ('a' :: 'b' :: Nil) |> takeMany(char().filter(Char.isLetter))
        ==
        Some((Nil, 'a' :: 'b' :: Nil))

    @test
    def takeMany2(): Bool = 
        use Parser.{takeMany, char, filter};
        ('a' :: 'b' :: '-' :: Nil) |> takeMany(char().filter(Char.isLetter))
        ==
        Some(('-' :: Nil, 'a' :: 'b' :: Nil))

    @test
    def takeMany3(): Bool = 
        use Parser.{takeMany, char, filter};

        ('a' :: 'b' :: '-' :: 'c' :: Nil) |> takeMany(char().filter(Char.isLetter))
        ==
        Some(('-' :: 'c' :: Nil, 'a' :: 'b' :: Nil))

    @test
    def takeMany4(): Bool = 
        use Parser.{takeMany, char, filter};

        ('_' :: 'a' :: 'b' :: 'c' :: Nil) |> takeMany(char().filter(Char.isLetter))
        ==
        None

    @test
    def takeMany5(): Bool = 
        use Parser.{takeMany, char, filter};
        Nil |> takeMany(char().filter(Char.isLetter)) == None

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // seperatedAny                                                                              //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def seperatedAny1(): Bool =
        use Parser.{seperatedAny, tag};
        String.toList("ababa") |> seperatedAny(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
       
    @test
    def seperatedAny2(): Bool =
        use Parser.{seperatedAny, tag};
        String.toList("abab") |> seperatedAny(tag('a'), tag('b'))
        ==
        Some(('b' :: Nil, 'a' :: 'a' :: Nil))
       
    @test
    def seperatedAny3(): Bool =
        use Parser.{seperatedAny, tag};
        String.toList("a") |> seperatedAny(tag('a'), tag('b')) == Some((Nil, 'a' :: Nil))
       
    @test
    def seperatedAny4(): Bool =
        use Parser.{seperatedAny, tag};
        String.toList("b") |> seperatedAny(tag('a'), tag('b')) == Some(('b' :: Nil, Nil))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // seperatedMany                                                                             //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def seperatedMany1(): Bool =
        use Parser.{seperatedMany, tag};
        String.toList("ababa") |> seperatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
       
    @test
    def seperatedMany2(): Bool =
        use Parser.{seperatedMany, tag};
        String.toList("abab") |> seperatedMany(tag('a'), tag('b'))
        ==
        Some(('b' :: Nil, 'a' :: 'a' :: Nil))
    @test
    def seperatedMany3(): Bool =
        use Parser.{seperatedMany, tag};
        String.toList("a") |> seperatedMany(tag('a'), tag('b')) == Some((Nil, 'a' :: Nil))
    @test
    def seperatedMany4(): Bool =
        use Parser.{seperatedMany, tag};
        String.toList("b") |> seperatedMany(tag('a'), tag('b')) == None

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // tailSeperatedAny                                                                          //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def tailSeperatedAny1(): Bool =
        use Parser.{tailSeperatedMany, tag};
        String.toList("ababa") |> tailSeperatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
       
    @test
    def tailSeperatedAny2(): Bool =
        use Parser.{tailSeperatedMany, tag};
        String.toList("abab") |> tailSeperatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: Nil))
       
    @test
    def tailSeperatedAny3(): Bool =
        use Parser.{tailSeperatedMany, tag};
        String.toList("a") |> tailSeperatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: Nil))
       
    @test
    def tailSeperatedAny4(): Bool =
        use Parser.{tailSeperatedMany, tag};
        String.toList("b") |> tailSeperatedMany(tag('a'), tag('b'))
        ==
        None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // tailSeperatedMany                                                                         //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def tailSeperatedMany1(): Bool =
        use Parser.{tailSeperatedMany, tag};
        String.toList("ababa") |> tailSeperatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
       
    @test
    def tailSeperatedMany2(): Bool =
        use Parser.{tailSeperatedMany, tag};
        String.toList("abab") |> tailSeperatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: 'a' :: Nil))
       
    @test
    def tailSeperatedMany3(): Bool =
        use Parser.{tailSeperatedMany, tag};
        String.toList("a") |> tailSeperatedMany(tag('a'), tag('b'))
        ==
        Some((Nil, 'a' :: Nil))
       
    @test
    def tailSeperatedMany4(): Bool =
        use Parser.{tailSeperatedMany, tag};
        String.toList("b") |> tailSeperatedMany(tag('a'), tag('b'))
        ==
        None
    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // delimit                                                                                   //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def delimit1(): Bool =
        use Parser.{delimit, tag, takeAny, char, filter};
        String.toList("[abz]") |>
            delimit(tag('['), takeAny(char().filter(Char.isLetter)), tag(']'))
        ==
        Some((Nil, 'a' :: 'b' :: 'z' :: Nil))
       
    @test
    def delimit2(): Bool =
        use Parser.{delimit, tag, takeAny, char, filter};
        String.toList("[a.bz]") |>
            delimit(tag('['), takeAny(char().filter(Char.isLetter)), tag(']'))
        ==
        None
       
    @test
    def delimit3(): Bool =
        use Parser.{delimit, tag, takeAny, char, filter};
        String.toList("[]") |>
            delimit(tag('['), takeAny(char().filter(Char.isLetter)), tag(']'))
        ==
        Some((Nil, Nil))

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // fold                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def fold1(): Bool =
        use Parser.{fold, char};
        String.toList("abc") |> fold(char(), Nil, l -> c -> c :: l)
        ==
        Some((Nil, 'c' :: 'b' :: 'a' :: Nil))
}

mod TestDebug {
    use Debug.stringify;

    /////////////////////////////////////////////////////////////////////////////
    // stringify                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testUnitStringify01(): Bool = stringify(()) == "()"


    @test
    def testBoolStringify01(): Bool = stringify(true) == "true"

    @test
    def testBoolStringify02(): Bool = stringify(false) == "false"


    @test
    def testCharStringify01(): Bool = stringify('a') == "\'a\'"

    @test
    def testCharStringify02(): Bool = stringify('\u2194') == "\'\u2194\'"

    @test
    def testCharStringify03(): Bool = stringify('\t') == "\'\\t\'"


    @test
    def testFloat32Stringify01(): Bool = stringify(21.42f32) == "21.42f32"


    @test
    def testInt8Stringify01(): Bool = stringify(42i8) == "42i8"


    @test
    def testInt16Stringify01(): Bool = stringify(42i16) == "42i16"


    @test
    def testInt32Stringify01(): Bool = stringify(42i32) == "42"


    @test
    def testInt64Stringify01(): Bool = stringify(42i64) == "42i64"


    @test
    def testBigIntStringify01(): Bool = stringify(42ii) == "42ii"


    @test
    def testStringStringify01(): Bool = stringify("Hello World!") == "\"Hello World!\""

    @test
    def testStringStringify02(): Bool = stringify("one\ttwo") == "\"one\\ttwo\""

    @test
    def testStringStringify03(): Bool = stringify("\u0000") == "\"\u0000\""

    @test
    def testStringStringify05(): Bool = stringify("\n\r\\\"\'\t\${\%{\u1234\u0000") ==
        "\"" + "\\n" + "\\r" + "\\\\" + "\\\"" + "\\\'" + "\\t" + "\${" + "\%{" + "\u1234" + "\u0000" + "\""


    @test
    def testEmptyArrayStringify01(): Bool \ IO = stringify(Array#{} @ Static) == "[]"

    @test
    def testArrayOfUnitStringify01(): Bool \ IO = stringify(Array#{(), ()} @ Static) == "[(), ()]"

    @test
    def testArrayOfBoolStringify01(): Bool \ IO = stringify(Array#{true, false} @ Static) == "[true, false]"


    @test
    def testArrayOfCharStringify01(): Bool \ IO = stringify(Array#{'a', 'b', 'c'} @ Static) == "['a', 'b', 'c']"


    @test
    def testArrayOfFloat32Stringify01(): Bool \ IO = stringify(Array#{21.42f32, 98.6f32} @ Static) == "[21.42f32, 98.6f32]"


    @test
    def testArrayOfInt8Stringify01(): Bool \ IO = stringify(Array#{42i8, 43i8} @ Static) == "[42i8, 43i8]"


    @test
    def testArrayOfInt16Stringify01(): Bool \ IO = stringify(Array#{42i16, 43i16} @ Static) == "[42i16, 43i16]"


    @test
    def testArrayOfInt32Stringify01(): Bool \ IO = stringify(Array#{42i32, 43i32} @ Static) == "[42, 43]"


    @test
    def testArrayOfInt64Stringify01(): Bool \ IO = stringify(Array#{42i64, 43i64} @ Static) == "[42i64, 43i64]"


    @test
    def testArrayOfBigIntStringify01(): Bool \ IO = stringify(Array#{42ii, 43ii} @ Static) == "[42ii, 43ii]"


    @test
    def testArrayOfStringStringify01(): Bool \ IO = stringify(Array#{"Hello", "World!"} @ Static) == "[\"Hello\", \"World!\"]"

    @test
    def testArrayOfVectorOfIntStringify01(): Bool \ IO = stringify(Array#{Vector#{1, 2}, Vector#{3, 4}} @ Static) == "[Vector#{1, 2}, Vector#{3, 4}]"

    @test
    def testArrayOfArrayOfIntStringify01(): Bool \ IO = stringify(Array#{Array#{1, 2} @ Static, Array#{3, 4} @ Static} @ Static) == "[[1, 2], [3, 4]]"


    @test
    def testArrayOfArrayOfArrayOfIntStringify01(): Bool \ IO = stringify(Array#{Array#{Array#{1, 2} @ Static, Array#{3, 4} @ Static} @ Static, Array#{Array#{5, 6} @ Static, Array#{7, 8} @ Static} @ Static} @ Static) == "[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]"


    @test
    def testEmptyVectorStringify01(): Bool = stringify(Vector#{}) == "Vector#{}"

    @test
    def testVectorOfUnitStringify01(): Bool = stringify(Vector#{(), ()}) == "Vector#{(), ()}"

    @test
    def testVectorOfBoolStringify01(): Bool = stringify(Vector#{true, false}) == "Vector#{true, false}"

    @test
    def testVectorOfStringStringify01(): Bool = stringify(Vector#{"Hello", "World!"}) == "Vector#{\"Hello\", \"World!\"}"

    @test
    def testVectorOfArrayOfIntStringify01(): Bool \ IO = stringify(Vector#{Array#{1, 2} @ Static, Array#{3, 4} @ Static}) == "Vector#{[1, 2], [3, 4]}"

    @test
    def testVectorOfVectorOfIntStringify01(): Bool = stringify(Vector#{Vector#{1, 2}, Vector#{3, 4}}) == "Vector#{Vector#{1, 2}, Vector#{3, 4}}"


    @test
    def testTupleStringify01(): Bool = stringify((true, 'a', "Hello World!")) == "(true, a, Hello World!)"

    @test
    def testTupleStringify02(): Bool = stringify((false, 2, ())) == "(false, 2, ())"


    pub enum Suit {
        case Hearts, Diamonds, Clubs, Spades
    }

    pub enum Rank {
        case N(Int32), Jack, Queen, King, Ace
    }

    pub enum Card {
        case Card(Suit, Rank)
    }

    @test
    def testEnumStringify01(): Bool = stringify(Card.Card(Suit.Hearts, Rank.N(5)))  == "Card(Hearts, N(5))"


    pub enum Poly[a, b] {
        case NoneOfThem,
        case This(a)
        case That(b)
        case Both(a, b)
    }

    @test
    def testPolyStringify01(): Bool = stringify(Poly.NoneOfThem) == "NoneOfThem"

    @test
    def testPolyStringify02(): Bool = stringify(Poly.This(123)) == "This(123)"

    @test
    def testPolyStringify03(): Bool = stringify(Poly.That("Hello World!")) == "That(Hello World!)"

    @test
    def testPolyStringify04(): Bool = stringify(Poly.Both(123, "Hello World!")) == "Both(123, Hello World!)"

    @test
    def testRecordStringify01(): Bool = stringify({field = (1, 2)}) == "{field = (1, 2)}"

    @test
    def testRecordStringify02(): Bool = {
        let s = stringify({something = (), other = true});
        s == "{something = (), other = true}" or s == "{other = true, something = ()}"
    }

    @test
    def testRecordStringify03(): Bool = {
        let s = stringify({something = {}, other = {single = 8}});
        s == "{something = {}, other = {single = 8}}" or s == "{other = {single = 8}, something = {}}"
    }

    @test
    def testFunctionStringify01(): Bool = {
        def f(x: Int32): Int32 = x;
        let s = stringify(f);
        s == "Int32 -> Int32"
    }

    def toIntFunction(x: a, g: a -> Int32): Int32 = g(x)

    @test
    def testFunctionStringify02(): Bool = {
        let s = stringify(toIntFunction(true));
        s == "Obj -> Int32"
    }

    def thunkFunction(x: a): String -> a = _ -> x

    @test
    def testFunctionStringify03(): Bool = {
        let s = stringify((thunkFunction(42), thunkFunction('a')));
        s == "(Obj -> Int32, Obj -> Char)"
    }

     @test
     def testNullStringify01(): Bool = {
         let s = stringify(null);
         s == "null"
     }

     @test
     def testNullStringify02(): Bool = {
         let s = stringify(unchecked_cast(null as String));
         s == "null"
     }

     @test
     def testNullStringify03(): Bool = {
         let s = stringify(unchecked_cast(null as BigInt));
         s == "null"
     }

    @test
    def testNullStringify04(): Bool = {
        let s = stringify(Poly.This(unchecked_cast(null as String)));
        s == "This"
    }

    @test
    def testNullStringify05(): Bool = {
        let s = stringify((null, 42));
        s == "(null, 42)"
    }

    @test
    def testNullStringify06(): Bool = region r {
        let s = stringify(Array#{unchecked_cast(null as String), "42"} @ r);
        s == "[null, \"42\"]"
    }

    @test
    def testNullStringify07(): Bool = {
        let s = stringify({field = unchecked_cast(null as String)});
        s == "{field = null}"
    }
}

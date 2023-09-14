mod TestDebug {
    use Debug.stringify;

    /////////////////////////////////////////////////////////////////////////////
    // stringify                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testUnitStringify01(): Bool = stringify(()) == "()"


    @Test
    def testBoolStringify01(): Bool = stringify(true) == "true"

    @Test
    def testBoolStringify02(): Bool = stringify(false) == "false"


    @Test
    def testCharStringify01(): Bool = stringify('a') == "\'a\'"

    @Test
    def testCharStringify02(): Bool = stringify('\u2194') == "\'\u2194\'"

    @Test
    def testCharStringify03(): Bool = stringify('\t') == "\'\\t\'"


    @Test
    def testFloat32Stringify01(): Bool = stringify(21.42f32) == "21.42f32"


    @Test
    def testInt8Stringify01(): Bool = stringify(42i8) == "42i8"


    @Test
    def testInt16Stringify01(): Bool = stringify(42i16) == "42i16"


    @Test
    def testInt32Stringify01(): Bool = stringify(42i32) == "42"


    @Test
    def testInt64Stringify01(): Bool = stringify(42i64) == "42i64"


    @Test
    def testBigIntStringify01(): Bool = stringify(42ii) == "42ii"


    @Test
    def testStringStringify01(): Bool = stringify("Hello World!") == "\"Hello World!\""

    @Test
    def testStringStringify02(): Bool = stringify("one\ttwo") == "\"one\\ttwo\""

    @Test
    def testStringStringify03(): Bool = stringify("\u0000") == "\"\u0000\""

    @Test
    def testStringStringify05(): Bool = stringify("\n\r\\\"\'\t\${\%{\u1234\u0000") ==
        "\"" + "\\n" + "\\r" + "\\\\" + "\\\"" + "\\\'" + "\\t" + "\${" + "\%{" + "\u1234" + "\u0000" + "\""

    @Test
    def testArrayOfUnitStringify01(): Bool \ IO = stringify(Array#{(), ()} @ Static) == "[(), ()]"


    @Test
    def testArrayOfBoolStringify01(): Bool \ IO = stringify(Array#{true, false} @ Static) == "[true, false]"


    @Test
    def testArrayOfCharStringify01(): Bool \ IO = stringify(Array#{'a', 'b', 'c'} @ Static) == "[a, b, c]"


    @Test
    def testArrayOfFloat32Stringify01(): Bool \ IO = stringify(Array#{21.42f32, 98.6f32} @ Static) == "[21.42, 98.6]"


    @Test
    def testArrayOfInt8Stringify01(): Bool \ IO = stringify(Array#{42i8, 43i8} @ Static) == "[42, 43]"


    @Test
    def testArrayOfInt16Stringify01(): Bool \ IO = stringify(Array#{42i16, 43i16} @ Static) == "[42, 43]"


    @Test
    def testArrayOfInt32Stringify01(): Bool \ IO = stringify(Array#{42i32, 43i32} @ Static) == "[42, 43]"


    @Test
    def testArrayOfInt64Stringify01(): Bool \ IO = stringify(Array#{42i64, 43i64} @ Static) == "[42, 43]"


    @Test
    def testArrayOfBigIntStringify01(): Bool \ IO = stringify(Array#{42ii, 43ii} @ Static) == "[42, 43]"


    @Test
    def testArrayOfStringStringify01(): Bool \ IO = stringify(Array#{"Hello", "World!"} @ Static) == "[Hello, World!]"


    @Test
    def testArrayOfArrayOfIntStringify01(): Bool \ IO = stringify(Array#{Array#{1, 2} @ Static, Array#{3, 4} @ Static} @ Static) == "[[1, 2], [3, 4]]"


    @Test
    def testArrayOfArrayOfArrayOfIntStringify01(): Bool \ IO = stringify(Array#{Array#{Array#{1, 2} @ Static, Array#{3, 4} @ Static} @ Static, Array#{Array#{5, 6} @ Static, Array#{7, 8} @ Static} @ Static} @ Static) == "[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]"


    @Test
    def testTupleStringify01(): Bool = stringify((true, 'a', "Hello World!")) == "(true, a, Hello World!)"

    @Test
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

    @Test
    def testEnumStringify01(): Bool = stringify(Card.Card(Suit.Hearts, Rank.N(5)))  == "Card(Hearts, N(5))"


    pub enum Poly[a, b] {
        case NoneOfThem,
        case This(a)
        case That(b)
        case Both(a, b)
    }

    @Test
    def testPolyStringify01(): Bool = stringify(Poly.NoneOfThem) == "NoneOfThem"

    @Test
    def testPolyStringify02(): Bool = stringify(Poly.This(123)) == "This(123)"

    @Test
    def testPolyStringify03(): Bool = stringify(Poly.That("Hello World!")) == "That(Hello World!)"

    @Test
    def testPolyStringify04(): Bool = stringify(Poly.Both(123, "Hello World!")) == "Both(123, Hello World!)"

    @Test
    def testRecordStringify01(): Bool = stringify({field = (1, 2)}) == "{field = (1, 2)}"

    @Test
    def testRecordStringify02(): Bool = {
        let s = stringify({something = (), other = true});
        s == "{something = (), other = true}" or s == "{other = true, something = ()}"
    }

    @Test
    def testRecordStringify03(): Bool = {
        let s = stringify({something = {}, other = {single = 8}});
        s == "{something = {}, other = {single = 8}}" or s == "{other = {single = 8}, something = {}}"
    }

    @Test
    def testFunctionStringify01(): Bool = {
        def f(x: Int32): Int32 = x;
        let s = stringify(f);
        s == "Int32 -> Int32"
    }

    def toIntFunction(x: a, g: a -> Int32): Int32 = g(x)

    @Test
    def testFunctionStringify02(): Bool = {
        let s = stringify(toIntFunction(true));
        s == "Obj -> Int32"
    }

    def thunkFunction(x: a): String -> a = _ -> x

    @Test
    def testFunctionStringify03(): Bool = {
        let s = stringify((thunkFunction(42), thunkFunction('a')));
        s == "(Obj -> Int32, Obj -> Char)"
    }

     @Test
     def testNullStringify01(): Bool = {
         let s = stringify(null);
         s == "null"
     }

     @Test
     def testNullStringify02(): Bool = {
         let s = stringify(unchecked_cast(null as String));
         s == "null"
     }

     @Test
     def testNullStringify03(): Bool = {
         let s = stringify(unchecked_cast(null as BigInt));
         s == "null"
     }

    @Test
    def testNullStringify04(): Bool = {
        let s = stringify(Poly.This(unchecked_cast(null as String)));
        s == "This(null)"
    }

    @Test
    def testNullStringify05(): Bool = {
        let s = stringify((null, 42));
        s == "(null, 42)"
    }

    @Test
    def testNullStringify06(): Bool = region rc {
        let s = stringify(Array#{unchecked_cast(null as String), "42"} @ rc);
        s == "[null, 42]"
    }

    @Test
    def testNullStringify07(): Bool = {
        let s = stringify({field = unchecked_cast(null as String)});
        s == "{field = null}"
    }
}

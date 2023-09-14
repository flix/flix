/*
 *  Copyright 2020 Matthew Lutze, Esben Bjerre
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

mod TestHash {

    use Hash.hash

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unitHash01(): Bool = hash(()) == hash(())

    /////////////////////////////////////////////////////////////////////////////
    // Bool                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def boolHash01(): Bool = hash(true) == hash(true)

    @Test
    def boolHash02(): Bool = hash(false) == hash(false)

    @Test
    def boolHash03(): Bool = hash(true) != hash(false)

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charHash01(): Bool = hash('Q') == hash('Q')

    @Test
    def charHash02(): Bool = hash('a') == hash('a')

    @Test
    def charHash03(): Bool = hash('z') == hash('z')

    @Test
    def charHash04(): Bool = hash(' ') == hash(' ')

    @Test
    def charHash05(): Bool = hash('/') != hash('#')

    @Test
    def charHash06(): Bool = hash('a') != hash('A')

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float32Hash01(): Bool = hash(123.0f32) == hash(123.0f32)

    @Test
    def float32Hash02(): Bool = hash(123.0f32) != hash(124.0f32)

    @Test
    def float32Hash03(): Bool = hash(0.0f32) == hash(0.0f32)

    @Test
    def float32Hash04(): Bool = hash(97.0f32) != hash(96.0f32)

    @Test
    def float32Hash05(): Bool = hash(123.123f32) != hash(123.0f32)

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float64Hash01(): Bool = hash(123.0f64) == hash(123.0f64)

    @Test
    def float64Hash02(): Bool = hash(123.0f64) != hash(124.0f64)

    @Test
    def float64Hash03(): Bool = hash(123.0f64) != hash(124.0f64)

    @Test
    def float64Hash04(): Bool = hash(0.0f64) == hash(0.0f64)

    @Test
    def float64Hash05(): Bool = hash(123.1f64) != hash(124.1f64)

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigDecimalHash01(): Bool = hash(123.0ff) == hash(123.0ff)

    @Test
    def bigDecimalHash02(): Bool = hash(123.0ff) != hash(124.0ff)

    @Test
    def bigDecimalHash03(): Bool = hash(123.0ff) != hash(124.0ff)

    @Test
    def bigDecimalHash04(): Bool = hash(0.0ff) == hash(0.0ff)

    @Test
    def bigDecimalHash05(): Bool = hash(123.1ff) != hash(124.1ff)

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int8Hash01(): Bool = hash(0i8) == hash(0i8)

    @Test
    def int8Hash02(): Bool = hash(-128i8) == hash(-128i8)

    @Test
    def int8Hash03(): Bool = hash(-128i8) != hash(127i8)

    @Test
    def int8Hash04(): Bool = hash(127i8) == hash(127i8)

    @Test
    def int8Hash05(): Bool = hash(1i8) != hash(2i8)

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int16Hash01(): Bool = hash(0i16) == hash(0i16)

    @Test
    def int16Hash02(): Bool = hash(-32768i16) == hash(-32768i16)

    @Test
    def int16Hash03(): Bool = hash(-32768i16) != hash(32767i16)

    @Test
    def int16Hash04(): Bool = hash(32767i16) == hash(32767i16)

    @Test
    def int16Hash05(): Bool = hash(1i16) != hash(2i16)

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int32Hash01(): Bool = hash(0) == hash(0)

    @Test
    def int32Hash02(): Bool = hash(-2147483648) == hash(-2147483648)

    @Test
    def int32Hash03(): Bool = hash(-2147483648) != hash(2147483647)

    @Test
    def int32Hash04(): Bool = hash(2147483647) == hash(2147483647)

    @Test
    def int32Hash05(): Bool = hash(1) != hash(2)

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int64Hash01(): Bool = hash(0i64) == hash(0i64)

    @Test
    def int64Hash02(): Bool = hash(-9223372036854775808i64) == hash(-9223372036854775808i64)

    @Test
    def int64Hash03(): Bool = hash(-9223372036854775808i64) != hash(9223372036854775806i64)

    @Test
    def int64Hash04(): Bool = hash(9223372036854775807i64) == hash(9223372036854775807i64)

    @Test
    def int64Hash05(): Bool = hash(1i64) != hash(2i64)

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stringHash01(): Bool = hash("abcd") == hash("abcd")

    @Test
    def stringHash02(): Bool = hash(" ") == hash(" ")

    @Test
    def stringHash03(): Bool = hash("Hello, World!") == hash("Hello, World!")

    @Test
    def stringHash04(): Bool = hash("Hello, World!") != hash("Hello World!")

    @Test
    def stringHash05(): Bool = hash("8BADF00D") != hash("8BADFOOD")

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigIntHash01(): Bool = hash(123ii) == hash(123ii)

    @Test
    def bigIntHash02(): Bool = hash(0ii) != hash(1ii)

    @Test
    def bigIntHash03(): Bool = hash(5719364391ii) == hash(5719364391ii)

    @Test
    def bigIntHash04(): Bool = hash(123ii) != hash(321ii)

    @Test
    def bigIntHash05(): Bool = hash(0ii) == hash(0ii)

    /////////////////////////////////////////////////////////////////////////////
    // Tuple2                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple2Hash01(): Bool = hash((1, 1)) == hash((1, 1))

    @Test
    def tuple2Hash02(): Bool = hash((1, 2)) != hash((2, 1))

    @Test
    def tuple2Hash03(): Bool = hash(((), ())) == hash(((), ()))

    @Test
    def tuple2Hash04(): Bool = hash((133i16, 'x')) == hash((133i16, 'x'))

    @Test
    def tuple2Hash05(): Bool = hash((true, true)) != hash((false, true))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple3                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple3Hash01(): Bool = hash((1, 1, 7)) == hash((1, 1, 7))

    @Test
    def tuple3Hash02(): Bool = hash((1, 2, ())) != hash((2, 1, ()))

    @Test
    def tuple3Hash03(): Bool = hash((1, 2, 3)) != hash((1, 3, 2))

    @Test
    def tuple3Hash04(): Bool = hash((133i16, 'x', "a")) == hash((133i16, 'x', "a"))

    @Test
    def tuple3Hash05(): Bool = hash((true, true, false)) != hash((false, true, false))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple4                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple4Hash01(): Bool = hash((1, 1, 7, 1 :: 2 :: Nil)) == hash((1, 1, 7, 1 :: 2 :: Nil))

    @Test
    def tuple4Hash02(): Bool = hash((1, 2, (), 9)) != hash((2, 1, (), 9))

    @Test
    def tuple4Hash03(): Bool = hash((1, 2, 3, 4)) != hash((1, 3, 2, 4))

    @Test
    def tuple4Hash04(): Bool = hash((133i16, 'x', "a", 0)) == hash((133i16, 'x', "a", 0))

    @Test
    def tuple4Hash05(): Bool = hash((true, true, false, false)) != hash((false, true, false, false))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple5                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple5Hash01(): Bool = hash((1, 1, 7, 1 :: 2 :: Nil, true)) == hash((1, 1, 7, 1 :: 2 :: Nil, true))

    @Test
    def tuple5Hash02(): Bool = hash((1, 2, (), 9, 8)) != hash((2, 1, (), 9, 8))

    @Test
    def tuple5Hash03(): Bool = hash((1, 2, 3, 4, 5)) != hash((1, 2, 4, 3, 5))

    @Test
    def tuple5Hash04(): Bool = hash((133i16, 'x', "a", 0, true)) == hash((133i16, 'x', "a", 0, true))

    @Test
    def tuple5Hash05(): Bool = hash((true, true, false, false, 'x')) != hash((false, true, false, false, 'x'))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple6                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple6Hash01(): Bool =
        hash((1, 1, 7, 1 :: 2 :: Nil, true, ())) == hash((1, 1, 7, 1 :: 2 :: Nil, true, ()))

    @Test
    def tuple6Hash02(): Bool =
        hash((1, 2, (), 9, 8, (1, 2, 3))) != hash((2, 1, (), 9, 8, (1, 2, 3)))

    @Test
    def tuple6Hash03(): Bool =
        hash((1, 2, 3, 4, 5, (1, 2))) != hash((1, 2, 4, 3, 5, (2, 1)))

    @Test
    def tuple6Hash04(): Bool =
        hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5))) == hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5)))

    @Test
    def tuple6Hash05(): Bool =
        hash((true, true, false, false, 'x', "rem")) != hash((false, true, false, false, 'x', "rem"))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple7                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple7Hash01(): Bool =
        hash((1, 1, 7, 1 :: 2 :: Nil, true, (), (9, 8, 6))) == hash((1, 1, 7, 1 :: 2 :: Nil, true, (), (9, 8, 6)))

    @Test
    def tuple7Hash02(): Bool =
        hash((1, 2, (), 9, 8, (1, 2, 3), true)) != hash((2, 1, (), 9, 8, (1, 2, 3), true))

    @Test
    def tuple7Hash03(): Bool =
        hash((1, 2, 3, 4, 5, (1, 2), (1, 2, 3))) != hash((1, 2, 4, 3, 5, (2, 1), (1, 3, 2)))

    @Test
    def tuple7Hash04(): Bool =
        hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5), 1337)) == hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5), 1337))

    @Test
    def tuple7Hash05(): Bool =
        hash((true, true, false, false, 'x', "rem", (1, 1, true))) != hash((false, true, false, false, 'x', "rem", (1, 1, true)))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple8                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple8Hash01(): Bool =
        hash((1, 1, 7, 1 :: 2 :: Nil, true, (), (9, 8, 6), (1, 2))) == hash((1, 1, 7, 1 :: 2 :: Nil, true, (), (9, 8, 6), (1, 2)))

    @Test
    def tuple8Hash02(): Bool =
        hash((1, 2, (), 9, 8, (1, 2, 3), true, "a-Z")) != hash((2, 1, (), 9, 8, (1, 2, 3), true, "a-Z"))

    @Test
    def tuple8Hash03(): Bool =
        hash((1, 2, 3, 4, 5, (1, 2), (1, 2, 3), (1, 2, 3, 4, 5))) != hash((1, 2, 4, 3, 5, (2, 1), (1, 3, 2), (1, 2, 3, "a", "b")))

    @Test
    def tuple8Hash04(): Bool =
        hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5), 1337, false)) == hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5), 1337, false))

    @Test
    def tuple8Hash05(): Bool =
        hash((true, true, false, false, 'x', "rem", (1, 1, true), (1, 2, 3, 9, 8))) != hash((false, true, false, false, 'x', "rem", (1, 1, true), (1, 2, 3, 9, 8)))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple9                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple9Hash01(): Bool =
        hash((1, 1, 7, 1 :: 2 :: Nil, true, (), (9, 8, 6), (1, 2), 37.5)) == hash((1, 1, 7, 1 :: 2 :: Nil, true, (), (9, 8, 6), (1, 2), 37.5))

    @Test
    def tuple9Hash02(): Bool =
        hash((1, 2, (), 9, 8, (1, 2, 3), true, "a-Z", ('a', 66i8))) != hash((2, 1, (), 9, 8, (1, 2, 3), true, "a-Z", ('a', 66i8)))

    @Test
    def tuple9Hash03(): Bool =
        hash((1, 2, 3, 4, 5, (1, 2), (1, 2, 3), (1, 2, 3, 4, 5), 'a')) != hash((1, 2, 4, 3, 5, (2, 1), (1, 3, 2), (1, 2, 3, "a", "b"), 'a'))

    @Test
    def tuple9Hash04(): Bool =
        hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5), 1337, false, (1, 3, 'a', 'z'))) == hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5), 1337, false, (1, 3, 'a', 'z')))

    @Test
    def tuple9Hash05(): Bool =
        hash((true, true, false, false, 'x', "rem", (1, 1, true), (1, 2, 3, 9, 8), '_')) != hash((false, true, false, false, 'x', "rem", (1, 1, true), (1, 2, 3, 9, 8), '_'))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple10                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple10Hash01(): Bool =
        hash((1, 1, 7, 1 :: 2 :: Nil, true, (), (9, 8, 6), (1, 2), 37.5, Set#{1, 2})) == hash((1, 1, 7, 1 :: 2 :: Nil, true, (), (9, 8, 6), (1, 2), 37.5, Set#{1, 2}))

    @Test
    def tuple10Hash02(): Bool =
        hash((1, 2, (), 9, 8, (1, 2, 3), true, "a-Z", ('a', 66i8), Set#{9, 7, 1})) != hash((2, 1, (), 9, 8, (1, 2, 3), true, "a-Z", ('a', 66i8), Set#{9, 7, 1}))

    @Test
    def tuple10Hash03(): Bool =
        hash((1, 2, 3, 4, 5, (1, 2), (1, 2, 3), (1, 2, 3, 4, 5), 'a', Map#{1 => 2, 2 => 1})) != hash((1, 2, 4, 3, 5, (2, 1), (1, 3, 2), (1, 2, 3, "a", "b"), 'a', Map#{1 => 2, 2 => 1}))

    @Test
    def tuple10Hash04(): Bool =
        hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5), 1337, false, (1, 3, 'a', 'z'), Map#{1 => 2, 2 => 1})) == hash((133i16, 'x', "a", 0, true, (1, 2, 4, 5), 1337, false, (1, 3, 'a', 'z'), Map#{1 => 2, 2 => 1}))

    @Test
    def tuple10Hash05(): Bool =
        hash((true, true, false, false, 'x', "rem", (1, 1, true), (1, 2, 3, 9, 8), '_', "tired")) != hash((false, true, false, false, 'x', "rem", (1, 1, true), (1, 2, 3, 9, 8), '_', "tired"))

}

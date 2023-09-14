/*
 *  Copyright 2020 Matthew Lutze
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

mod TestToString {

    use ToString.toString

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unitToString01(): Bool = toString(()) == "()"

    /////////////////////////////////////////////////////////////////////////////
    // Bool                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def boolToString01(): Bool = toString(false) == "false"

    @Test
    def boolToString02(): Bool = toString(true) == "true"

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charToString01(): Bool = toString('a') == "a"

    @Test
    def charToString02(): Bool = toString('A') == "A"

    @Test
    def charToString03(): Bool = toString(' ') == " "

    @Test
    def charToString04(): Bool = toString('+') == "+"

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float32ToString01(): Bool = toString(0.0f32) == "0.0"

    @Test
    def float32ToString02(): Bool = toString(1.0f32) == "1.0"

    @Test
    def float32ToString03(): Bool = toString(-1.0f32) == "-1.0"

    @Test
    def float32ToString04(): Bool = toString(3.14f32) == "3.14"

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float64ToString01(): Bool = toString(0.0f64) == "0.0"

    @Test
    def float64ToString02(): Bool = toString(1.0f64) == "1.0"

    @Test
    def float64ToString03(): Bool = toString(-1.0f64) == "-1.0"

    @Test
    def float64ToString04(): Bool = toString(3.14f64) == "3.14"

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigDecimalToString01(): Bool = toString(0.0ff) == "0.0"

    @Test
    def bigDecimalToString02(): Bool = toString(1.0ff) == "1.0"

    @Test
    def bigDecimalToString03(): Bool = toString(-1.0ff) == "-1.0"

    @Test
    def bigDecimalToString04(): Bool = toString(3.14ff) == "3.14"

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int8ToString01(): Bool = toString(0i8) == "0"

    @Test
    def int8ToString02(): Bool = toString(1i8) == "1"

    @Test
    def int8ToString03(): Bool = toString(-128i8) == "-128"

    @Test
    def int8ToString04(): Bool = toString(127i8) == "127"

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int16ToString01(): Bool = toString(0i16) == "0"

    @Test
    def int16ToString02(): Bool = toString(1i16) == "1"

    @Test
    def int16ToString03(): Bool = toString(-32768i16) == "-32768"

    @Test
    def int16ToString04(): Bool = toString(32767i16) == "32767"

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int32ToString01(): Bool = toString(0) == "0"

    @Test
    def int32ToString02(): Bool = toString(1) == "1"

    @Test
    def int32ToString03(): Bool = toString(-2147483648) == "-2147483648"

    @Test
    def int32ToString04(): Bool = toString(2147483647) == "2147483647"

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int64ToString01(): Bool = toString(0i64) == "0"

    @Test
    def int64ToString02(): Bool = toString(1i64) == "1"

    @Test
    def int64ToString03(): Bool = toString(-9223372036854775808i64) == "-9223372036854775808"

    @Test
    def int64ToString04(): Bool = toString(9223372036854775807i64) == "9223372036854775807"

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stringToString01(): Bool = toString("") == ""

    @Test
    def stringToString02(): Bool = toString("a") == "a"

    @Test
    def stringToString03(): Bool = toString(".") == "."

    @Test
    def stringToString04(): Bool = toString(".#)!933") == ".#)!933"

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigIntToString01(): Bool = toString(0ii) == "0"

    @Test
    def bigIntToString02(): Bool = toString(1ii) == "1"

    @Test
    def bigIntToString03(): Bool = toString(-3498457932459234592873452983453245ii) == "-3498457932459234592873452983453245"

    @Test
    def bigIntToString04(): Bool = toString(3498457932459234592873452983453245ii) == "3498457932459234592873452983453245"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple2                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple2ToString01(): Bool = toString((1, 1)) == "(1, 1)"

    @Test
    def tuple2ToString02(): Bool = toString((1, Some(2))) == "(1, Some(2))"

    @Test
    def tuple2ToString03(): Bool = toString((true, 1)) == "(true, 1)"

    @Test
    def tuple2ToString04(): Bool = toString(((1, 2), 91)) == "((1, 2), 91)"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple3                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple3ToString01(): Bool = toString((1, 1, 82)) == "(1, 1, 82)"

    @Test
    def tuple3ToString02(): Bool = toString((1, Some(2), 3)) == "(1, Some(2), 3)"

    @Test
    def tuple3ToString03(): Bool = toString((true, 1, false)) == "(true, 1, false)"

    @Test
    def tuple3ToString04(): Bool = toString(((1, 2), 91, true)) == "((1, 2), 91, true)"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple4                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple4ToString01(): Bool =
        toString((1, 1, 2, 3)) == "(1, 1, 2, 3)"

    @Test
    def tuple4ToString02(): Bool =
        toString((1, Some(2), true, false)) == "(1, Some(2), true, false)"

    @Test
    def tuple4ToString03(): Bool =
        toString((true, 1, 97, 2)) == "(true, 1, 97, 2)"

    @Test
    def tuple4ToString04(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4)) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4)"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple5                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple5ToString01(): Bool =
        toString((1, 1, 2, 3, 9)) == "(1, 1, 2, 3, 9)"

    @Test
    def tuple5ToString02(): Bool =
        toString((1, Some(2), true, false, true)) == "(1, Some(2), true, false, true)"

    @Test
    def tuple5ToString03(): Bool =
        toString((true, 1, 97, 2, 9)) == "(true, 1, 97, 2, 9)"

    @Test
    def tuple5ToString04(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple6                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple6ToString01(): Bool =
        toString((1, 1, 2, 3, 9, 42)) == "(1, 1, 2, 3, 9, 42)"

    @Test
    def tuple6ToString02(): Bool =
        toString((1, Some(2), true, false, true, 0)) == "(1, Some(2), true, false, true, 0)"

    @Test
    def tuple6ToString03(): Bool =
        toString((true, 1, 97, 2, 9, false)) == "(true, 1, 97, 2, 9, false)"

    @Test
    def tuple6ToString04(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple7                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple7ToString01(): Bool =
        toString((1, 1, 2, 3, 9, 42, 2)) == "(1, 1, 2, 3, 9, 42, 2)"

    @Test
    def tuple7ToString02(): Bool =
        toString((1, Some(2), true, false, true, 0, false)) == "(1, Some(2), true, false, true, 0, false)"

    @Test
    def tuple7ToString03(): Bool =
        toString((true, 1, 97, 2, 9, false, Some(42))) == "(true, 1, 97, 2, 9, false, Some(42))"

    @Test
    def tuple7ToString04(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple8                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple8ToString01(): Bool =
        toString((1, 1, 2, 3, 9, 42, 2, 0)) == "(1, 1, 2, 3, 9, 42, 2, 0)"

    @Test
    def tuple8ToString02(): Bool =
        toString((1, Some(2), true, false, true, 0, false, Some(true))) == "(1, Some(2), true, false, true, 0, false, Some(true))"

    @Test
    def tuple8ToString03(): Bool =
        toString((true, 1, 97, 2, 9, false, Some(42), 1i64)) == "(true, 1, 97, 2, 9, false, Some(42), 1)"

    @Test
    def tuple8ToString04(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple9                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple9ToString01(): Bool =
        toString((1, 1, 2, 3, 9, 42, 2, 0, 9)) == "(1, 1, 2, 3, 9, 42, 2, 0, 9)"

    @Test
    def tuple9ToString02(): Bool =
        toString((1, Some(2), true, false, true, 0, false, Some(true), 1)) == "(1, Some(2), true, false, true, 0, false, Some(true), 1)"

    @Test
    def tuple9ToString03(): Bool =
        toString((true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32)) == "(true, 1, 97, 2, 9, false, Some(42), 1, 432)"

    @Test
    def tuple9ToString04(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple10                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple10ToString01(): Bool =
        toString((1, 1, 2, 3, 9, 42, 2, 0, 9, 342)) == "(1, 1, 2, 3, 9, 42, 2, 0, 9, 342)"

    @Test
    def tuple10ToString02(): Bool =
        toString((1, Some(2), true, false, true, 0, false, Some(true), 1, 1337)) == "(1, Some(2), true, false, true, 0, false, Some(true), 1, 1337)"

    @Test
    def tuple10ToString03(): Bool =
        toString((true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32, Some(false))) == "(true, 1, 97, 2, 9, false, Some(42), 1, 432, Some(false))"

    @Test
    def tuple10ToString04(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false, true))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false, true))"

}

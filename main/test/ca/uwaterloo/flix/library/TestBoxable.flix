/*
 * Copyright 2021 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace TestBoxable {

    use Boxable.{box, unbox};

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unitBox01(): Bool = unbox(box(())) == ()

    /////////////////////////////////////////////////////////////////////////////
    // Bool                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def boolBox01(): Bool = unbox(box(false)) == false

    @test
    def boolBox02(): Bool = unbox(box(true)) == true

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def charBox01(): Bool = unbox(box('a')) == 'a'

    @test
    def charBox02(): Bool = unbox(box('A')) == 'A'

    @test
    def charBox03(): Bool = unbox(box(' ')) == ' '

    @test
    def charBox04(): Bool = unbox(box('+')) == '+'

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float32Box01(): Bool = unbox(box(0.0f32)) == 0.0f32

    @test
    def float32Box02(): Bool = unbox(box(1.0f32)) == 1.0f32

    @test
    def float32Box03(): Bool = unbox(box(-1.0f32)) == -1.0f32

    @test
    def float32Box04(): Bool = unbox(box(3.14f32)) == 3.14f32

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float64Box01(): Bool = unbox(box(0.0f64)) == 0.0f64

    @test
    def float64Box02(): Bool = unbox(box(1.0f64)) == 1.0f64

    @test
    def float64Box03(): Bool = unbox(box(-1.0f64)) == -1.0f64

    @test
    def float64Box04(): Bool = unbox(box(3.14f64)) == 3.14f64

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int8Box01(): Bool = unbox(box(0i8)) == 0i8

    @test
    def int8Box02(): Bool = unbox(box(1i8)) == 1i8

    @test
    def int8Box03(): Bool = unbox(box(-128i8)) == -128i8

    @test
    def int8Box04(): Bool = unbox(box(127i8)) == 127i8

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int16Box01(): Bool = unbox(box(0i16)) == 0i16

    @test
    def int16Box02(): Bool = unbox(box(1i16)) == 1i16

    @test
    def int16Box03(): Bool = unbox(box(-32768i16)) == -32768i16

    @test
    def int16Box04(): Bool = unbox(box(32767i16)) == 32767i16

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int32Box01(): Bool = unbox(box(0)) == 0i32

    @test
    def int32Box02(): Bool = unbox(box(1)) == 1i32

    @test
    def int32Box03(): Bool = unbox(box(-2147483648)) == -2147483648i32

    @test
    def int32Box04(): Bool = unbox(box(2147483647)) == 2147483647i32

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int64Box01(): Bool = unbox(box(0i64)) == 0i64

    @test
    def int64Box02(): Bool = unbox(box(1i64)) == 1i64

    @test
    def int64Box03(): Bool = unbox(box(-9223372036854775808i64)) == -9223372036854775808i64

    @test
    def int64Box04(): Bool = unbox(box(9223372036854775807i64)) == 9223372036854775807i64

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stringBox01(): Bool = unbox(box("")) == ""

    @test
    def stringBox02(): Bool = unbox(box("a")) == "a"

    @test
    def stringBox03(): Bool = unbox(box(".")) == "."

    @test
    def stringBox04(): Bool = unbox(box(".#)!933")) == ".#)!933"

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bigIntBox01(): Bool = unbox(box(0ii)) == 0ii

    @test
    def bigIntBox02(): Bool = unbox(box(1ii)) == 1ii

    @test
    def bigIntBox03(): Bool = unbox(box(-3498457932459234592873452983453245ii)) == -3498457932459234592873452983453245ii

    @test
    def bigIntBox04(): Bool = unbox(box(3498457932459234592873452983453245ii)) == 3498457932459234592873452983453245ii

    /////////////////////////////////////////////////////////////////////////////
    // Tuple2                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple2Box01(): Bool = unbox(box((1, 1))) == (1, 1)

    @test
    def tuple2Box02(): Bool = unbox(box((1, Some(2)))) == (1, Some(2))

    @test
    def tuple2Box03(): Bool = unbox(box((true, 1))) == (true, 1)

    @test
    def tuple2Box04(): Bool = unbox(box(((1, 2), 91))) == ((1, 2), 91)

    /////////////////////////////////////////////////////////////////////////////
    // Tuple3                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple3Box01(): Bool = unbox(box((1, 1, 82))) == (1, 1, 82)

    @test
    def tuple3Box02(): Bool = unbox(box((1, Some(2), 3))) == (1, Some(2), 3)

    @test
    def tuple3Box03(): Bool = unbox(box((true, 1, false))) == (true, 1, false)

    @test
    def tuple3Box04(): Bool = unbox(box(((1, 2), 91, true))) == ((1, 2), 91, true)

    /////////////////////////////////////////////////////////////////////////////
    // Tuple4                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple4Box01(): Bool =
        unbox(box((1, 1, 2, 3))) == (1, 1, 2, 3)

    @test
    def tuple4Box02(): Bool =
        unbox(box((1, Some(2), true, false))) == (1, Some(2), true, false)

    @test
    def tuple4Box03(): Bool =
        unbox(box((true, 1, 97, 2))) == (true, 1, 97, 2)

    @test
    def tuple4Box04(): Bool =
        unbox(box(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4))) == ((1, 2, 4, 7), (1, 2), (1, 2, 4), 4)

    /////////////////////////////////////////////////////////////////////////////
    // Tuple5                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple5Box01(): Bool =
        unbox(box((1, 1, 2, 3, 9))) == (1, 1, 2, 3, 9)

    @test
    def tuple5Box02(): Bool =
        unbox(box((1, Some(2), true, false, true))) == (1, Some(2), true, false, true)

    @test
    def tuple5Box03(): Bool =
        unbox(box((true, 1, 97, 2, 9))) == (true, 1, 97, 2, 9)

    @test
    def tuple5Box04(): Bool =
        unbox(box(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5)))) == ((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple6                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple6Box01(): Bool =
        unbox(box((1, 1, 2, 3, 9, 42))) == (1, 1, 2, 3, 9, 42)

    @test
    def tuple6Box02(): Bool =
        unbox(box((1, Some(2), true, false, true, 0))) == (1, Some(2), true, false, true, 0)

    @test
    def tuple6Box03(): Bool =
        unbox(box((true, 1, 97, 2, 9, false))) == (true, 1, 97, 2, 9, false)

    @test
    def tuple6Box04(): Bool =
        unbox(box(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9)))) == ((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple7                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple7Box01(): Bool =
        unbox(box((1, 1, 2, 3, 9, 42, 2))) == (1, 1, 2, 3, 9, 42, 2)

    @test
    def tuple7Box02(): Bool =
        unbox(box((1, Some(2), true, false, true, 0, false))) == (1, Some(2), true, false, true, 0, false)

    @test
    def tuple7Box03(): Bool =
        unbox(box((true, 1, 97, 2, 9, false, Some(42)))) == (true, 1, 97, 2, 9, false, Some(42))

    @test
    def tuple7Box04(): Bool =
        unbox(box(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1)))) == ((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple8                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple8Box01(): Bool =
        unbox(box((1, 1, 2, 3, 9, 42, 2, 0))) == (1, 1, 2, 3, 9, 42, 2, 0)

    @test
    def tuple8Box02(): Bool =
        unbox(box((1, Some(2), true, false, true, 0, false, Some(true)))) == (1, Some(2), true, false, true, 0, false, Some(true))

    @test
    def tuple8Box03(): Bool =
        unbox(box((true, 1, 97, 2, 9, false, Some(42), 1i64))) == (true, 1, 97, 2, 9, false, Some(42), 1i64)

    @test
    def tuple8Box04(): Bool =
        unbox(box(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42)))) == ((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple9                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple9Box01(): Bool =
        unbox(box((1, 1, 2, 3, 9, 42, 2, 0, 9))) == (1, 1, 2, 3, 9, 42, 2, 0, 9)

    @test
    def tuple9Box02(): Bool =
        unbox(box((1, Some(2), true, false, true, 0, false, Some(true), 1))) == (1, Some(2), true, false, true, 0, false, Some(true), 1)

    @test
    def tuple9Box03(): Bool =
        unbox(box((true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32))) == (true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32)

    @test
    def tuple9Box04(): Bool =
        unbox(box(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false)))) == ((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple10                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple10Box01(): Bool =
        unbox(box((1, 1, 2, 3, 9, 42, 2, 0, 9, 342))) == (1, 1, 2, 3, 9, 42, 2, 0, 9, 342)

    @test
    def tuple10Box02(): Bool =
        unbox(box((1, Some(2), true, false, true, 0, false, Some(true), 1, 1337))) == (1, Some(2), true, false, true, 0, false, Some(true), 1, 1337)

    @test
    def tuple10Box03(): Bool =
        unbox(box((true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32, Some(false)))) == (true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32, Some(false))

    @test
    def tuple10Box04(): Bool =
        unbox(box(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false, true)))) == ((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false, true))

}
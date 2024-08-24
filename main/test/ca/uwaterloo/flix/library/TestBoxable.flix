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

mod TestBoxable {

    use Fixpoint.Boxable.{box, unbox}

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unitUnbox01(): Bool = unbox(box(())) == ()

    @test
    def unitCompare01(): Bool = box(()) == box(())

    @test
    def unitToString01(): Bool = "${box(())}" == "()"

    /////////////////////////////////////////////////////////////////////////////
    // Bool                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def boolUnbox01(): Bool = unbox(box(false)) == false

    @test
    def boolUnbox02(): Bool = unbox(box(true)) == true

    @test
    def boolCompare01(): Bool = box(true) == box(true)

    @test
    def boolCompare02(): Bool = box(false) < box(true)

    @test
    def boolToString01(): Bool = "${box(true)}" == "true"

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def charUnbox01(): Bool = unbox(box('a')) == 'a'

    @test
    def charUnbox02(): Bool = unbox(box('A')) == 'A'

    @test
    def charUnbox03(): Bool = unbox(box(' ')) == ' '

    @test
    def charUnbox04(): Bool = unbox(box('+')) == '+'

    @test
    def charCompare01(): Bool = box('a') == box('a')

    @test
    def charCompare02(): Bool = box('a') < box('b')

    @test
    def charToString01(): Bool = "${box('V')}" == "V"

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float32Unbox01(): Bool = unbox(box(0.0f32)) == 0.0f32

    @test
    def float32Unbox02(): Bool = unbox(box(1.0f32)) == 1.0f32

    @test
    def float32Unbox03(): Bool = unbox(box(-1.0f32)) == -1.0f32

    @test
    def float32Unbox04(): Bool = unbox(box(3.14f32)) == 3.14f32

    @test
    def float32Compare01(): Bool = box(0.0f32) == box(0.0f32)

    @test
    def float32Compare02(): Bool = box(0.0f32) < box(1.0f32)

    @test
    def float32ToString01(): Bool = "${box(123.0f32)}" == "123.0"

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float64Unbox01(): Bool = unbox(box(0.0f64)) == 0.0f64

    @test
    def float64Unbox02(): Bool = unbox(box(1.0f64)) == 1.0f64

    @test
    def float64Unbox03(): Bool = unbox(box(-1.0f64)) == -1.0f64

    @test
    def float64Unbox04(): Bool = unbox(box(3.14f64)) == 3.14f64

    @test
    def float64Compare01(): Bool = box(0.0f64) == box(0.0f64)

    @test
    def float64Compare02(): Bool = box(0.0f64) < box(1.0f64)

    @test
    def float64ToString01(): Bool = "${box(123.0f64)}" == "123.0"

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bigDecimalUnbox01(): Bool = unbox(box(0.0ff)) == 0.0ff

    @test
    def bigDecimalUnbox02(): Bool = unbox(box(1.0ff)) == 1.0ff

    @test
    def bigDecimalUnbox03(): Bool = unbox(box(-1.0ff)) == -1.0ff

    @test
    def bigDecimalUnbox04(): Bool = unbox(box(3.14ff)) == 3.14ff

    @test
    def bigDecimalCompare01(): Bool = box(0.0ff) == box(0.0ff)

    @test
    def bigDecimalCompare02(): Bool = box(0.0ff) < box(1.0ff)

    @test
    def bigDecimalToString01(): Bool = "${box(123.0ff)}" == "123.0"

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int8Unbox01(): Bool = unbox(box(0i8)) == 0i8

    @test
    def int8Unbox02(): Bool = unbox(box(1i8)) == 1i8

    @test
    def int8Unbox03(): Bool = unbox(box(-128i8)) == -128i8

    @test
    def int8Unbox04(): Bool = unbox(box(127i8)) == 127i8

    @test
    def int8Compare01(): Bool = box(0i8) == box(0i8)

    @test
    def int8Compare02(): Bool = box(0i8) < box(1i8)

    @test
    def int8ToString01(): Bool = "${box(123i8)}" == "123"

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int16Unbox01(): Bool = unbox(box(0i16)) == 0i16

    @test
    def int16Unbox02(): Bool = unbox(box(1i16)) == 1i16

    @test
    def int16Unbox03(): Bool = unbox(box(-32768i16)) == -32768i16

    @test
    def int16Unbox04(): Bool = unbox(box(32767i16)) == 32767i16

    @test
    def int16Compare01(): Bool = box(0i16) == box(0i16)

    @test
    def int16Compare02(): Bool = box(0i16) < box(1i16)

    @test
    def int16ToString01(): Bool = "${box(123i16)}" == "123"

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int32Unbox01(): Bool = unbox(box(0)) == 0i32

    @test
    def int32Unbox02(): Bool = unbox(box(1)) == 1i32

    @test
    def int32Unbox03(): Bool = unbox(box(-2147483648)) == -2147483648i32

    @test
    def int32Unbox04(): Bool = unbox(box(2147483647)) == 2147483647i32

    @test
    def int32Compare01(): Bool = box(0i32) == box(0i32)

    @test
    def int32Compare02(): Bool = box(0i32) < box(1i32)

    @test
    def int32ToString01(): Bool = "${box(123i32)}" == "123"

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int64Unbox01(): Bool = unbox(box(0i64)) == 0i64

    @test
    def int64Unbox02(): Bool = unbox(box(1i64)) == 1i64

    @test
    def int64Unbox03(): Bool = unbox(box(-9223372036854775808i64)) == -9223372036854775808i64

    @test
    def int64Unbox04(): Bool = unbox(box(9223372036854775807i64)) == 9223372036854775807i64

    @test
    def int64Compare01(): Bool = box(0i64) == box(0i64)

    @test
    def int64Compare02(): Bool = box(0i64) < box(1i64)

    @test
    def int64ToString01(): Bool = "${box(123i64)}" == "123"

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stringUnbox01(): Bool = unbox(box("")) == ""

    @test
    def stringUnbox02(): Bool = unbox(box("a")) == "a"

    @test
    def stringUnbox03(): Bool = unbox(box(".")) == "."

    @test
    def stringUnbox04(): Bool = unbox(box(".#)!933")) == ".#)!933"

    @test
    def stringCompare01(): Bool = box("") == box("")

    @test
    def stringCompare02(): Bool = box("a") < box("b")

    @test
    def stringToString01(): Bool = "${box("abc")}" == "abc"

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bigIntUnbox01(): Bool = unbox(box(0ii)) == 0ii

    @test
    def bigIntUnbox02(): Bool = unbox(box(1ii)) == 1ii

    @test
    def bigIntUnbox03(): Bool = unbox(box(-3498457932459234592873452983453245ii)) == -3498457932459234592873452983453245ii

    @test
    def bigIntUnbox04(): Bool = unbox(box(3498457932459234592873452983453245ii)) == 3498457932459234592873452983453245ii

    @test
    def bigIntCompare01(): Bool = box(0ii) == box(0ii)

    @test
    def bigIntCompare02(): Bool = box(0ii) < box(1ii)

    @test
    def bigIntToString01(): Bool = "${box(123ii)}" == "123"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple2                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple2Unbox01(): Bool = unbox(box((1, 1))) == (1, 1)

    @test
    def tuple2Unbox02(): Bool = unbox(box((1, Some(2)))) == (1, Some(2))

    @test
    def tuple2Unbox03(): Bool = unbox(box((true, 1))) == (true, 1)

    @test
    def tuple2Unbox04(): Bool = unbox(box(((1, 2), 91))) == ((1, 2), 91)

    @test
    def tuple2Compare01(): Bool = box((0, 0)) == box((0, 0))

    @test
    def tuple2Compare02(): Bool = box((0, 0)) < box((0, 1))

    @test
    def tuple2ToString01(): Bool = "${box((1, 2))}" == "(1, 2)"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple3                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple3Unbox01(): Bool = unbox(box((1, 1, 82))) == (1, 1, 82)

    @test
    def tuple3Unbox02(): Bool = unbox(box((1, Some(2), 3))) == (1, Some(2), 3)

    @test
    def tuple3Unbox03(): Bool = unbox(box((true, 1, false))) == (true, 1, false)

    @test
    def tuple3Unbox04(): Bool = unbox(box(((1, 2), 91, true))) == ((1, 2), 91, true)

    @test
    def tuple3Compare01(): Bool = box((0, 0, 0)) == box((0, 0, 0))

    @test
    def tuple3Compare02(): Bool = box((0, 0, 0)) < box((0, 0, 1))

    @test
    def tuple3ToString01(): Bool = "${box((1, 2, 3))}" == "(1, 2, 3)"
}

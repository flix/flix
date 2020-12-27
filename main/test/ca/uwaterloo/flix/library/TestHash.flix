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

namespace TestHash {

    use Hash.hash;

    @test
    def unitHash01(): Bool = hash(()) == hash(())

    @test
    def boolHash01(): Bool = hash(true) == hash(true)

    @test
    def boolHash02(): Bool = hash(false) == hash(false)

    @test
    def boolHash03(): Bool = hash(true) != hash(false)

    @test
    def charHash01(): Bool = hash('Q') == hash('Q')

    @test
    def charHash02(): Bool = hash('a') == hash('a')

    @test
    def charHash03(): Bool = hash('z') == hash('z')

    @test
    def charHash04(): Bool = hash(' ') == hash(' ')

    @test
    def charHash05(): Bool = hash('/') != hash('#')

    @test
    def charHash06(): Bool = hash('a') != hash('A')

    @test
    def float32Hash01(): Bool = hash(123.0f32) == hash(123.0f32)

    @test
    def float32Hash02(): Bool = hash(123.0f32) != hash(124.0f32)

    @test
    def float32Hash03(): Bool = hash(0.0f32) == hash(0.0f32)

    @test
    def float32Hash04(): Bool = hash(97.0f32) != hash(96.0f32)

    @test
    def float32Hash05(): Bool = hash(123.123f32) != hash(123.0f32)

    @test
    def float64Hash01(): Bool = hash(123.0f64) == hash(123.0f64)

    @test
    def float64Hash02(): Bool = hash(123.0f64) != hash(124.0f64)

    @test
    def float64Hash03(): Bool = hash(123.0f64) != hash(124.0f64)

    @test
    def float64Hash04(): Bool = hash(0.0f64) == hash(0.0f64)

    @test
    def float64Hash05(): Bool = hash(123.1f64) != hash(124.1f64)

    @test
    def int8Hash01(): Bool = hash(0i8) == hash(0i8)

    @test
    def int8Hash02(): Bool = hash(-128i8) == hash(-128i8)

    @test
    def int8Hash03(): Bool = hash(-128i8) != hash(127i8)

    @test
    def int8Hash04(): Bool = hash(127i8) == hash(127i8)

    @test
    def int8Hash05(): Bool = hash(1i8) != hash(2i8)

    @test
    def int16Hash01(): Bool = hash(0i16) == hash(0i16)

    @test
    def int16Hash02(): Bool = hash(-32768i16) == hash(-32768i16)

    @test
    def int16Hash03(): Bool = hash(-32768i16) != hash(32767i16)

    @test
    def int16Hash04(): Bool = hash(32767i16) == hash(32767i16)

    @test
    def int16Hash05(): Bool = hash(1i16) != hash(2i16)

    @test
    def int32Hash01(): Bool = hash(0) == hash(0)

    @test
    def int32Hash02(): Bool = hash(-2147483648) == hash(-2147483648)

    @test
    def int32Hash03(): Bool = hash(-2147483648) != hash(2147483647)

    @test
    def int32Hash04(): Bool = hash(2147483647) == hash(2147483647)

    @test
    def int32Hash05(): Bool = hash(1) != hash(2)

    @test
    def int64Hash01(): Bool = hash(0i64) == hash(0i64)

    @test
    def int64Hash02(): Bool = hash(-9223372036854775808i64) == hash(-9223372036854775808i64)

    @test
    def int64Hash03(): Bool = hash(-9223372036854775808i64) != hash(9223372036854775806i64)

    @test
    def int64Hash04(): Bool = hash(9223372036854775807i64) == hash(9223372036854775807i64)

    @test
    def int64Hash05(): Bool = hash(1i64) != hash(2i64)

    @test
    def stringHash01(): Bool = hash("abcd") == hash("abcd")

    @test
    def stringHash02(): Bool = hash(" ") == hash(" ")

    @test
    def stringHash03(): Bool = hash("Hello, World!") == hash("Hello, World!")

    @test
    def stringHash04(): Bool = hash("Hello, World!") != hash("Hello World!")

    @test
    def stringHash05(): Bool = hash("8BADF00D") != hash("8BADFOOD")

    @test
    def bigIntHash01(): Bool = hash(123ii) == hash(123ii)

    @test
    def bigIntHash02(): Bool = hash(0ii) != hash(1ii)

    @test
    def bigIntHash03(): Bool = hash(5719364391ii) == hash(5719364391ii)

    @test
    def bigIntHash04(): Bool = hash(123ii) != hash(321ii)

    @test
    def bigIntHash05(): Bool = hash(0ii) == hash(0ii)

}

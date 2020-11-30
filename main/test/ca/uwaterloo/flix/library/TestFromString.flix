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

namespace TestFromString {
    use FromString.fromString;

    @test
    def testFromStringUnit(): Bool = fromString("()") == Some(())

    @test
    def testFromStringTrue(): Bool = fromString("true") == Some(true)

    @test
    def testFromStringFalse(): Bool = fromString("false") == Some(false)

    @test
    def testFromStringChar(): Bool = fromString("Q") == Some('Q')

    @test
    def testFromStringFloat32(): Bool = fromString("123.0") == Some(123.0f32)

    @test
    def testFromStringFloat64(): Bool = fromString("123.0") == Some(123.0f64)

    @test
    def testFromStringInt8(): Bool = fromString("123") == Some(123i8)

    @test
    def testFromStringInt16(): Bool = fromString("123") == Some(123i16)

    @test
    def testFromStringInt32(): Bool = fromString("123") == Some(123i32)

    @test
    def testFromStringInt64(): Bool = fromString("123") == Some(123i64)

    @test
    def testFromStringString(): Bool = fromString("abcd") == Some("abcd")

    @test
    def testFromStringBigInt(): Bool = fromString("123") == Some(123ii)
}

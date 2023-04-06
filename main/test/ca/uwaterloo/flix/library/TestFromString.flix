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

mod TestFromString {
    use FromString.fromString

    @test
    def testFromStringUnitGood(): Bool = fromString("()") == Some(())

    @test
    def testFromStringUnitBad(): Bool = fromString(")(") == None: Option[Unit]

    @test
    def testFromStringTrueGood(): Bool = fromString("true") == Some(true)

    @test
    def testFromStringTrueBad(): Bool = fromString("fue") == None: Option[Bool]

    @test
    def testFromStringFalseGood(): Bool = fromString("false") == Some(false)

    @test
    def testFromStringFalseBad(): Bool = fromString("tralse") == None: Option[Bool]

    @test
    def testFromStringCharGood(): Bool = fromString("Q") == Some('Q')

    @test
    def testFromStringCharBad(): Bool = fromString("ABC") == None: Option[Char]

    @test
    def testFromStringFloat32Good(): Bool = fromString("123.0") == Some(123.0f32)

    @test
    def testFromStringFloat32Bad(): Bool = fromString("ABC") == None: Option[Float32]

    @test
    def testFromStringFloat64Good(): Bool = fromString("123.0") == Some(123.0f64)

    @test
    def testFromStringFloat64Bad(): Bool = fromString("ABC") == None: Option[Float64]

    @test
    def testFromStringBigDecimalGood(): Bool = fromString("123.0") == Some(123.0ff)

    @test
    def testFromStringBigDecimalBad(): Bool = fromString("ABC") == None: Option[BigDecimal]

    @test
    def testFromStringInt8Good(): Bool = fromString("123") == Some(123i8)

    @test
    def testFromStringInt8Bad(): Bool = fromString("ABC") == None: Option[Int8]

    @test
    def testFromStringInt16Good(): Bool = fromString("123") == Some(123i16)

    @test
    def testFromStringInt16Bad(): Bool = fromString("ABC") == None: Option[Int16]

    @test
    def testFromStringInt32Good(): Bool = fromString("123") == Some(123i32)

    @test
    def testFromStringInt32Bad(): Bool = fromString("ABC") == None: Option[Int32]

    @test
    def testFromStringInt64Good(): Bool = fromString("123") == Some(123i64)

    @test
    def testFromStringInt64Bad(): Bool = fromString("ABC") == None: Option[Int64]

    @test
    def testFromStringStringGood(): Bool = fromString("abcd") == Some("abcd")

    @test
    def testFromStringBigIntGood(): Bool = fromString("123") == Some(123ii)

    @test
    def testFromStringBigIntBad(): Bool = fromString("ABC") == None: Option[BigInt]

}

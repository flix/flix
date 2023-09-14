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

    @Test
    def testFromStringUnitGood(): Bool = fromString("()") == Some(())

    @Test
    def testFromStringUnitBad(): Bool = fromString(")(") == None: Option[Unit]

    @Test
    def testFromStringTrueGood(): Bool = fromString("true") == Some(true)

    @Test
    def testFromStringTrueBad(): Bool = fromString("fue") == None: Option[Bool]

    @Test
    def testFromStringFalseGood(): Bool = fromString("false") == Some(false)

    @Test
    def testFromStringFalseBad(): Bool = fromString("tralse") == None: Option[Bool]

    @Test
    def testFromStringCharGood(): Bool = fromString("Q") == Some('Q')

    @Test
    def testFromStringCharBad(): Bool = fromString("ABC") == None: Option[Char]

    @Test
    def testFromStringFloat32Good(): Bool = fromString("123.0") == Some(123.0f32)

    @Test
    def testFromStringFloat32Bad(): Bool = fromString("ABC") == None: Option[Float32]

    @Test
    def testFromStringFloat64Good(): Bool = fromString("123.0") == Some(123.0f64)

    @Test
    def testFromStringFloat64Bad(): Bool = fromString("ABC") == None: Option[Float64]

    @Test
    def testFromStringBigDecimalGood(): Bool = fromString("123.0") == Some(123.0ff)

    @Test
    def testFromStringBigDecimalBad(): Bool = fromString("ABC") == None: Option[BigDecimal]

    @Test
    def testFromStringInt8Good(): Bool = fromString("123") == Some(123i8)

    @Test
    def testFromStringInt8Bad(): Bool = fromString("ABC") == None: Option[Int8]

    @Test
    def testFromStringInt16Good(): Bool = fromString("123") == Some(123i16)

    @Test
    def testFromStringInt16Bad(): Bool = fromString("ABC") == None: Option[Int16]

    @Test
    def testFromStringInt32Good(): Bool = fromString("123") == Some(123i32)

    @Test
    def testFromStringInt32Bad(): Bool = fromString("ABC") == None: Option[Int32]

    @Test
    def testFromStringInt64Good(): Bool = fromString("123") == Some(123i64)

    @Test
    def testFromStringInt64Bad(): Bool = fromString("ABC") == None: Option[Int64]

    @Test
    def testFromStringStringGood(): Bool = fromString("abcd") == Some("abcd")

    @Test
    def testFromStringBigIntGood(): Bool = fromString("123") == Some(123ii)

    @Test
    def testFromStringBigIntBad(): Bool = fromString("ABC") == None: Option[BigInt]

}

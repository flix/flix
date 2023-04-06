/*
 *  Copyright 2020 Stephen Tetley
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

mod TestLowerBound {
    use LowerBound.minValue

    @test
    def testUnit(): Bool = minValue() == ()

    @test
    def testBool(): Bool = minValue() == false

    @test
    def testInt8(): Bool = minValue() == -128i8

    @test
    def testInt16(): Bool = minValue() == -32_768i16

    @test
    def testInt32(): Bool = minValue() == -2_147_483_648

    @test
    def testInt64(): Bool = minValue() == -9_223_372_036_854_775_808i64

    @test
    def testFloat32(): Bool = minValue() == Float32.minValue()

    @test
    def testFloat64(): Bool = minValue() == Float64.minValue()

    @test
    def testChar(): Bool = minValue() == Char.minValue()

    @test
    def testTuple2(): Bool = minValue() == ('\u0000', '\u0000')

    @test
    def testTuple3(): Bool = minValue() == ('\u0000', '\u0000', '\u0000')

    @test
    def testTuple4(): Bool = minValue() == ('\u0000', '\u0000', '\u0000', '\u0000')

    @test
    def testTuple5(): Bool = minValue() == ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000')

    @test
    def testTuple6(): Bool = minValue() == ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000')

    @test
    def testTuple7(): Bool = minValue() == ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000')

    @test
    def testTuple8(): Bool = minValue() == ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000')

    @test
    def testTuple9(): Bool = minValue() == ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000')

    @test
    def testTuple10(): Bool = minValue() == ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000')

}

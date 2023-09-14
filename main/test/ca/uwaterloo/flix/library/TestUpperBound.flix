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

mod TestUpperBound {
    use UpperBound.maxValue

    @Test
    def testUnit(): Bool = maxValue() == ()

    @Test
    def testBool(): Bool = maxValue() == true

    @Test
    def testInt8(): Bool = maxValue() == 127i8

    @Test
    def testInt16(): Bool = maxValue() == 32_767i16

    @Test
    def testInt32(): Bool = maxValue() == 2_147_483_647

    @Test
    def testInt64(): Bool = maxValue() == 9_223_372_036_854_775_807i64

    @Test
    def testFloat32(): Bool = maxValue() == Float32.maxValue()

    @Test
    def testFloat64(): Bool = maxValue() == Float64.maxValue()

    @Test
    def testChar(): Bool = maxValue() == '\uffff'

    @Test
    def testTuple2(): Bool = maxValue() == ('\uffff', '\uffff')

    @Test
    def testTuple3(): Bool = maxValue() == ('\uffff', '\uffff', '\uffff')

    @Test
    def testTuple4(): Bool = maxValue() == ('\uffff', '\uffff', '\uffff', '\uffff')

    @Test
    def testTuple5(): Bool = maxValue() == ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff')

    @Test
    def testTuple6(): Bool = maxValue() == ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff')

    @Test
    def testTuple7(): Bool = maxValue() == ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff')

    @Test
    def testTuple8(): Bool = maxValue() == ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff')

    @Test
    def testTuple9(): Bool = maxValue() == ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff')

    @Test
    def testTuple10(): Bool = maxValue() == ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff')

}

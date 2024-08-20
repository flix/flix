/*
 *  Copyright 2021 Jonathan Lindegaard Starup
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

mod Test.Exp.Discard {


    // Simple Type Tests


    @test
    def testBoolDiscard01(): Unit \ IO =
        let arr = Array#{true} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testCharDiscard01(): Unit \ IO =
        let arr = Array#{'q'} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testFloat32Discard01(): Unit \ IO =
        let arr = Array#{32.0f32} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testFloat64Discard01(): Unit \ IO =
        let arr = Array#{64.0f64} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testBigDecimalDiscard01(): Unit \ IO =
        let arr = Array#{9999999.0ff} @ Static;
        discard $ARRAY_LOAD$(arr, 0)


    @test
    def testInt8Discard01(): Unit \ IO =
        let arr = Array#{8i8} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testInt16Discard01(): Unit \ IO =
        let arr = Array#{16i16} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testInt32Discard01(): Unit \ IO =
        let arr = Array#{32i32} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testInt64Discard01(): Unit \ IO =
        let arr = Array#{64i64} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testBigIntDiscard01(): Unit \ IO =
        let arr = Array#{9999999ii} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testStringDiscard01(): Unit \ IO =
        let arr = Array#{"cool string"} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testArrayOfIntDiscard01(): Unit \ IO =
        let arr = Array#{Array#{2} @ Static} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testArrayOfStringDiscard01(): Unit \ IO =
        let arr = Array#{Array#{"cool string"} @ Static} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    @test
    def testTupleDiscard01(): Unit \ IO =
        let arr = Array#{("fst", "snd")} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    enum Enum {
        case Data(Int32)
    }

    @test
    def testEnumDiscard01(): Unit \ IO =
        let arr = Array#{Enum.Data(32)} @ Static;
        discard $ARRAY_LOAD$(arr, 0)

    pub enum Poly[a, b] {
            case None,
            case This(a)
            case That(b)
            case Both(a, b)
        }

    @test
    def testPolyDiscard01(): Unit \ IO =
        let arr = Array#{Poly.That("hello")} @ Static;
        discard $ARRAY_LOAD$(arr, 0)


    // Longer tests


    @test
    def testBuilderDiscard01(): Bool = region rc {
        let counter = Ref.fresh(rc, 0);
        def increaseBy(i, c) = {
            Ref.put(deref c + i, c);
            c
        };
        discard counter |> increaseBy(2) |> increaseBy(5);
        deref counter == 7
    }

}

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

namespace Test/Exp/Discard {

    // Simple Type Tests

    @test
    def testBoolDiscard01(): Unit & Impure =
        let arr = [true];
        discard arr[0]

    @test
    def testCharDiscard01(): Unit & Impure =
        let arr = ['q'];
        discard arr[0]

    @test
    def testFloat32Discard01(): Unit & Impure =
        let arr = [32.0f32];
        discard arr[0]

    @test
    def testFloat64Discard01(): Unit & Impure =
        let arr = [64.0f64];
        discard arr[0]

    @test
    def testInt8Discard01(): Unit & Impure =
        let arr = [8i8];
        discard arr[0]

    @test
    def testInt16Discard01(): Unit & Impure =
        let arr = [16i16];
        discard arr[0]

    @test
    def testInt32Discard01(): Unit & Impure =
        let arr = [32i32];
        discard arr[0]

    @test
    def testInt64Discard01(): Unit & Impure =
        let arr = [64i64];
        discard arr[0]

    @test
    def testBigIntDiscard01(): Unit & Impure =
        let arr = [9999999ii];
        discard arr[0]

    @test
    def testStringDiscard01(): Unit & Impure =
        let arr = ["cool string"];
        discard arr[0]

    @test
    def testArrayOfIntDiscard01(): Unit & Impure =
        let arr = [[2]];
        discard arr[0]

    @test
    def testArrayOfStringDiscard01(): Unit & Impure =
        let arr = [["cool string"]];
        discard arr[0]

    @test
    def testTupleDiscard01(): Unit & Impure =
        let arr = [("fst", "snd")];
        discard arr[0]

    enum Enum {
        case Data(Int32)
    }

    @test
    def testEnumDiscard01(): Unit & Impure =
        let arr = [Data(32)];
        discard arr[0]

    pub enum Poly[a, b] {
            case None,
            case This(a)
            case That(b)
            case Both(a, b)
        }

    @test
    def testPolyDiscard01(): Unit & Impure =
        let arr = [That("hello")];
        discard arr[0]

    // Longer tests

}

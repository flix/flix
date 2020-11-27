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

namespace TestRead {
    use Read.read;

    @test
    def testReadString(): Bool = read("abcd") == Ok("abcd")

    @test
    def testReadInt8(): Bool = read("123") == Ok(123i8)

    @test
    def testReadInt16(): Bool = read("123") == Ok(123i16)

    @test
    def testReadInt32(): Bool = read("123") == Ok(123i32)

    @test
    def testReadInt64(): Bool = read("123") == Ok(123i64)

    @test
    def testReadFloat32(): Bool = read("123.0") == Ok(123.0f32)

    @test
    def testReadFloat64(): Bool = read("123.0") == Ok(123.0f64)
}

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

namespace TestShow {
    use Show.show;

//    @test
//    def testShowUnit(): Bool = show(()) == "()"

    @test
    def testShowTrue(): Bool = show(true) == "true"

    @test
    def testShowFalse(): Bool = show(false) == "false"

    @test
    def testShowString(): Bool = show("abcd") == "abcd"

    @test
    def testShowInt8(): Bool = show(123i8) == "123"

    @test
    def testShowInt16(): Bool = show(123i16) == "123"

    @test
    def testShowInt32(): Bool = show(123i32) == "123"

    @test
    def testShowInt64(): Bool = show(123i64) == "123"

    @test
    def testShowFloat32(): Bool = show(123.0f32) == "123.0"

    @test
    def testShowFloat64(): Bool = show(123.0f64) == "123.0"

    @test
    def testShowInt32List(): Bool = show(1 :: 2 :: 3 :: Nil) == "1 :: 2 :: 3 :: Nil"

    @test
    def testShowNil(): Bool = show(Nil) == "Nil" // MATT this only currently works because we have Show[Unit]

    @test
    def testShowSomeInt32(): Bool = show(Some(123)) == "Some(123)"

    @test
    def testShowNone(): Bool = show(None) == "None"
}

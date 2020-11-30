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

namespace TestToString {
    use ToString.toString;

    @test
    def testToStringUnit(): Bool = toString(()) == "()"

    @test
    def testToStringTrue(): Bool = toString(true) == "true"

    @test
    def testToStringFalse(): Bool = toString(false) == "false"

    @test
    def testToStringChar(): Bool = toString('Q') == "Q"

    @test
    def testToStringFloat32(): Bool = toString(123.0f32) == "123.0"

    @test
    def testToStringFloat64(): Bool = toString(123.0f64) == "123.0"

    @test
    def testToStringInt8(): Bool = toString(123i8) == "123"

    @test
    def testToStringInt16(): Bool = toString(123i16) == "123"

    @test
    def testToStringInt32(): Bool = toString(123i32) == "123"

    @test
    def testToStringInt64(): Bool = toString(123i64) == "123"

    @test
    def testToStringString(): Bool = toString("abcd") == "abcd"

    @test
    def testToStringBigInt(): Bool = toString(123ii) == "123"

    @test
    def testToStringInt32List(): Bool = toString(1 :: 2 :: 3 :: Nil) == "1 :: 2 :: 3 :: Nil"

    @test
    def testToStringNil(): Bool = toString(Nil: List[Int]) == "Nil"

    @test
    def testToStringSomeInt32(): Bool = toString(Some(123)) == "Some(123)"

    @test
    def testToStringNone(): Bool = toString(None: Option[String]) == "None"

    @test
    def testToStringErr(): Bool = toString(Err("error"): Result[String, String]) == "Err(error)"

    @test
    def testToStringOk(): Bool = toString(Ok("all good"): Result[String, String]) == "Ok(all good)"

    @test
    def testToStringSuccess(): Bool = toString(Success("hooray"): Validation[String, String]) == "Success(hooray)"

    @test
    def testToStringNel(): Bool = toString(Nel("abc", Nil)) == "abc :: Nil"

    @test
    def testToStringNest01(): Bool =
        toString(Some(None) :: None :: Some(Some(Ok(123))) :: Some(Some(Err(false))) :: Nil) == "Some(None) :: None :: Some(Some(Ok(123))) :: Some(Some(Err(false))) :: Nil"

    @test
    def testToStringNest02(): Bool =
        toString(Some(Nil) :: None :: Some(Success(true) :: Failure(Nel("zut", Nil)) :: Nil) :: Nil) == "Some(Nil) :: None :: Some(Success(true) :: Failure(zut :: Nil) :: Nil) :: Nil"

    @test
    def testToStringNest03(): Bool =
        toString(Nil :: (1 :: Nil) :: (1 :: 2 :: Nil) :: Nil) == "Nil :: 1 :: Nil :: 1 :: 2 :: Nil :: Nil"
}

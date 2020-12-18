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

namespace TestHash {
    use Hash/Hash.hash;

    @test
    def testHashUnit(): Bool = hash(()) == hash(())

    @test
    def testHashTrue(): Bool = hash(true) == hash(true)

    @test
    def testHashFalse(): Bool = hash(false) == hash(false)

    @test
    def testHashBoolDiff(): Bool = hash(true) != hash(false)

    @test
    def testHashCharSame(): Bool = hash('Q') == hash('Q')

    @test
    def testHashCharDiff(): Bool = hash('Q') != hash('P')

    @test
    def testHashFloat32Same(): Bool = hash(123.0f32) == hash(123.0f32)

    @test
    def testHashFloat32Diff(): Bool = hash(123.0f32) != hash(124.0f32)

    @test
    def testHashFloat64Same(): Bool = hash(123.0f64) == hash(123.0f64)

    @test
    def testHashFloat64Diff(): Bool = hash(123.0f64) != hash(124.0f64)

    @test
    def testHashInt8Same(): Bool = hash(123i8) == hash(123i8)

    @test
    def testHashInt8Diff(): Bool = hash(123i8) != hash(124i8)

    @test
    def testHashInt16Same(): Bool = hash(123i16) == hash(123i16)

    @test
    def testHashInt16Diff(): Bool = hash(123i16) != hash(124i16)

    @test
    def testHashInt32Same(): Bool = hash(123i32) == hash(123i32)

    @test
    def testHashInt32Diff(): Bool = hash(123i32) != hash(124i32)

    @test
    def testHashInt64Same(): Bool = hash(123i64) == hash(123i64)

    @test
    def testHashInt64Diff(): Bool = hash(123i64) != hash(124i64)

    @test
    def testHashStringSame(): Bool = hash("abcd") == hash("abcd")

    @test
    def testHashStringDiff(): Bool = hash("abcd") != hash("efgh")

    @test
    def testHashBigIntSame(): Bool = hash(123ii) == hash(123ii)

    @test
    def testHashBigIntDiff(): Bool = hash(123ii) != hash(124ii)

    @test
    def testHashInt32ListSame(): Bool = hash(1 :: 2 :: 3 :: Nil) == hash(1 :: 2 :: 3 :: Nil)

    @test
    def testHashInt32ListDiff(): Bool = hash(1 :: 2 :: 3 :: Nil) != hash(3 :: 2 :: 1 :: Nil)

    @test
    def testHashNil(): Bool = hash(Nil: List[Int]) == hash(Nil: List[Int])

    @test
    def testHashSomeInt32Same(): Bool = hash(Some(123)) == hash(Some(123))

    @test
    def testHashSomeInt32Diff(): Bool = hash(Some(123)) != hash(Some(124))

    @test
    def testHashNone(): Bool = hash(None: Option[String]) == hash(None: Option[String])

    @test
    def testHashErrSame(): Bool = hash(Err("error"): Result[String, String]) == hash(Err("error"): Result[String, String])

    @test
    def testHashErrDiff(): Bool = hash(Err("error"): Result[String, String]) != hash(Err("error2"): Result[String, String])

    @test
    def testHashOkSame(): Bool = hash(Ok("all good"): Result[String, String]) == hash(Ok("all good"): Result[String, String])

    @test
    def testHashOkDiff(): Bool = hash(Ok("all good"): Result[String, String]) != hash(Ok("all good 2"): Result[String, String])

    @test
    def testHashSuccessSame(): Bool = hash(Success("hooray"): Validation[String, String]) == hash(Success("hooray"): Validation[String, String])

    @test
    def testHashSuccessDiff(): Bool = hash(Success("hooray"): Validation[String, String]) != hash(Success("hooray!"): Validation[String, String])

    @test
    def testHashNelSame(): Bool = hash(Nel("abc", Nil)) == hash(Nel("abc", Nil))

    @test
    def testHashNelDiff(): Bool = hash(Nel("abc", Nil)) != hash(Nel("def", Nil))

    @test
    def testHashNestSame01(): Bool =
        hash(Some(None) :: None :: Some(Some(Ok(123))) :: Some(Some(Err(false))) :: Nil) == hash(Some(None) :: None :: Some(Some(Ok(123))) :: Some(Some(Err(false))) :: Nil)

    @test
    def testHashNestDiff01(): Bool =
        hash(Some(None) :: None :: Some(Some(Ok(123))) :: Some(Some(Err(false))) :: Nil) != hash(Some(None) :: None :: Some(Some(Ok(123))) :: Some(Some(Err(true))) :: Nil)

    @test
    def testHashNestSame02(): Bool =
        hash(Some(Nil) :: None :: Some(Success(true) :: Failure(Nel("zut", Nil)) :: Nil) :: Nil) == hash(Some(Nil) :: None :: Some(Success(true) :: Failure(Nel("zut", Nil)) :: Nil) :: Nil)

    @test
    def testHashNestDiff02(): Bool =
        hash(Some(Nil) :: None :: Some(Success(true) :: Failure(Nel("zut", Nil)) :: Nil) :: Nil) != hash(Some(Nil) :: None :: Some(Success(true) :: Failure(Nel("flute", Nil)) :: Nil) :: Nil)

    @test
    def testHashNestSame03(): Bool =
        hash(Nil :: (1 :: Nil) :: (1 :: 2 :: Nil) :: Nil) == hash(Nil :: (1 :: Nil) :: (1 :: 2 :: Nil) :: Nil)

    @test
    def testHashNestDiff03(): Bool =
        hash(Nil :: (1 :: Nil) :: (1 :: 2 :: Nil) :: Nil) != hash(Nil :: (1 :: Nil) :: (1 :: 2 :: 3 :: Nil) :: Nil)
}

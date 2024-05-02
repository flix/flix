/*
 * Copyright 2023 Jakob Schneider Villumsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

 mod Test.Exp.CheckedEffectCast {

    enum A[a](a)

    @test
    def testCheckedEffectCast01(): Unit \ IO = ()

    @test
    def testCheckedEffectCast02(): Bool \ IO = true

    @test
    def testCheckedEffectCast03(): Bool \ IO = not false

    @test
    def testCheckedEffectCast04(): Char \ IO = 'a'

    @test
    def testCheckedEffectCast05(): Char \ IO = 'b'

    @test
    def testCheckedEffectCast06(): Char \ IO = 'Z'

    @test
    def testCheckedEffectCast07(): Char \ IO = '0'

    @test
    def testCheckedEffectCast08(): Float32 \ IO = 0.0f32

    @test
    def testCheckedEffectCast09(): Float32 \ IO = 123.4f32

    @test
    def testCheckedEffectCast10(): Float64 \ IO = 0.0f64

    @test
    def testCheckedEffectCast11(): Float64 \ IO = 123.4f64

    @test
    def testCheckedEffectCast12(): Int8 \ IO = 0i8

    @test
    def testCheckedEffectCast13(): Int8 \ IO = 64i8

    @test
    def testCheckedEffectCast14(): Int16 \ IO = 0i16

    @test
    def testCheckedEffectCast15(): Int16 \ IO = 64i16

    @test
    def testCheckedEffectCast16(): Int32 \ IO = 0i32

    @test
    def testCheckedEffectCast17(): Int32 \ IO = 64i32

    @test
    def testCheckedEffectCast18(): Int64 \ IO = 0i64

    @test
    def testCheckedEffectCast19(): Int64 \ IO = 64i64

    @test
    def testCheckedEffectCast20(): BigInt \ IO = 0ii

    @test
    def testCheckedEffectCast21(): BigInt \ IO = 64ii

    @test
    def testCheckedEffectCast22(): String \ IO = "ABC"

    @test
    def testCheckedEffectCast23(): Unit \ IO = region rc {
        discard $ARRAY_NEW$(rc, 'A', 8);
        ()
    }

    @test
    def testCheckedEffectCast24(): Unit \ IO = region rc {
        discard $ARRAY_NEW$(rc, Some(42), 8);
        ()
    }

    @test
    def testCheckedEffectCast25(): (Char, Int32, Bool) \ IO = ('a', 42, false)

    @test
    def testCheckedEffectCast26(): A[String] \ IO = A.A("ABC")

    @test
    def testCheckedEffectCast27(): (a -> a \ IO) = x -> x

    @test
    def testCheckedEffectCast28(): (a -> a) \ IO = x -> x

    pub eff E

    @test
    def testCheckedEffectCast29(): (a -> a \ E) = x -> x

}

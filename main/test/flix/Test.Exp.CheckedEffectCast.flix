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
    def testCheckedEffectCast01(): Unit \ IO = checked_ecast(())

    @test
    def testCheckedEffectCast02(): Bool \ IO = checked_ecast(true)

    @test
    def testCheckedEffectCast03(): Bool \ IO = checked_ecast(not false)

    @test
    def testCheckedEffectCast04(): Char \ IO = checked_ecast('a')

    @test
    def testCheckedEffectCast05(): Char \ IO = checked_ecast('b')

    @test
    def testCheckedEffectCast06(): Char \ IO = checked_ecast('Z')

    @test
    def testCheckedEffectCast07(): Char \ IO = checked_ecast('0')

    @test
    def testCheckedEffectCast08(): Float32 \ IO = checked_ecast(0.0f32)

    @test
    def testCheckedEffectCast09(): Float32 \ IO = checked_ecast(123.4f32)

    @test
    def testCheckedEffectCast10(): Float64 \ IO = checked_ecast(0.0f64)

    @test
    def testCheckedEffectCast11(): Float64 \ IO = checked_ecast(123.4f64)

    @test
    def testCheckedEffectCast12(): Int8 \ IO = checked_ecast(0i8)

    @test
    def testCheckedEffectCast13(): Int8 \ IO = checked_ecast(64i8)

    @test
    def testCheckedEffectCast14(): Int16 \ IO = checked_ecast(0i16)

    @test
    def testCheckedEffectCast15(): Int16 \ IO = checked_ecast(64i16)

    @test
    def testCheckedEffectCast16(): Int32 \ IO = checked_ecast(0i32)

    @test
    def testCheckedEffectCast17(): Int32 \ IO = checked_ecast(64i32)

    @test
    def testCheckedEffectCast18(): Int64 \ IO = checked_ecast(0i64)

    @test
    def testCheckedEffectCast19(): Int64 \ IO = checked_ecast(64i64)

    @test
    def testCheckedEffectCast20(): BigInt \ IO = checked_ecast(0ii)

    @test
    def testCheckedEffectCast21(): BigInt \ IO = checked_ecast(64ii)

    @test
    def testCheckedEffectCast22(): String \ IO = checked_ecast("ABC")

    @test
    def testCheckedEffectCast23(): Unit \ IO = region rc {
        discard checked_ecast($ARRAY_NEW$(rc, 'A', 8));
        ()
    }

    @test
    def testCheckedEffectCast24(): Unit \ IO = region rc {
        discard checked_ecast($ARRAY_NEW$(rc, Some(42), 8));
        ()
    }

    @test
    def testCheckedEffectCast25(): (Char, Int32, Bool) \ IO = checked_ecast(('a', 42, false))

    @test
    def testCheckedEffectCast26(): A[String] \ IO = checked_ecast(A.A("ABC"))

    @test
    def testCheckedEffectCast27(): (a -> a \ IO) = x -> checked_ecast(x)

    @test
    def testCheckedEffectCast28(): (a -> a) \ IO = checked_ecast(x -> x)

    pub eff E

    @test
    def testCheckedEffectCast29(): (a -> a \ E) = x -> checked_ecast(x)

}

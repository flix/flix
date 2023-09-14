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

    @Test
    def testCheckedEffectCast01(): Unit \ IO = checked_ecast(())

    @Test
    def testCheckedEffectCast02(): Bool \ IO = checked_ecast(true)

    @Test
    def testCheckedEffectCast03(): Bool \ IO = checked_ecast(not false)

    @Test
    def testCheckedEffectCast04(): Char \ IO = checked_ecast('a')

    @Test
    def testCheckedEffectCast05(): Char \ IO = checked_ecast('b')

    @Test
    def testCheckedEffectCast06(): Char \ IO = checked_ecast('Z')

    @Test
    def testCheckedEffectCast07(): Char \ IO = checked_ecast('0')

    @Test
    def testCheckedEffectCast08(): Float32 \ IO = checked_ecast(0.0f32)

    @Test
    def testCheckedEffectCast09(): Float32 \ IO = checked_ecast(123.4f32)

    @Test
    def testCheckedEffectCast10(): Float64 \ IO = checked_ecast(0.0f64)

    @Test
    def testCheckedEffectCast11(): Float64 \ IO = checked_ecast(123.4f64)

    @Test
    def testCheckedEffectCast12(): Int8 \ IO = checked_ecast(0i8)

    @Test
    def testCheckedEffectCast13(): Int8 \ IO = checked_ecast(64i8)

    @Test
    def testCheckedEffectCast14(): Int16 \ IO = checked_ecast(0i16)

    @Test
    def testCheckedEffectCast15(): Int16 \ IO = checked_ecast(64i16)

    @Test
    def testCheckedEffectCast16(): Int32 \ IO = checked_ecast(0i32)

    @Test
    def testCheckedEffectCast17(): Int32 \ IO = checked_ecast(64i32)

    @Test
    def testCheckedEffectCast18(): Int64 \ IO = checked_ecast(0i64)

    @Test
    def testCheckedEffectCast19(): Int64 \ IO = checked_ecast(64i64)

    @Test
    def testCheckedEffectCast20(): BigInt \ IO = checked_ecast(0ii)

    @Test
    def testCheckedEffectCast21(): BigInt \ IO = checked_ecast(64ii)

    @Test
    def testCheckedEffectCast22(): String \ IO = checked_ecast("ABC")

    @Test
    def testCheckedEffectCast23(): Unit \ IO = region rc {
        discard checked_ecast($ARRAY_NEW$(rc, 'A', 8));
        ()
    }

    @Test
    def testCheckedEffectCast24(): Unit \ IO = region rc {
        discard checked_ecast($ARRAY_NEW$(rc, Some(42), 8));
        ()
    }

    @Test
    def testCheckedEffectCast25(): (Char, Int32, Bool) \ IO = checked_ecast(('a', 42, false))

    @Test
    def testCheckedEffectCast26(): A[String] \ IO = checked_ecast(A.A("ABC"))

    @Test
    def testCheckedEffectCast27(): (a -> a \ IO) = x -> checked_ecast(x)

    @Test
    def testCheckedEffectCast28(): (a -> a) \ IO = checked_ecast(x -> x)

    pub eff E

    @Test
    def testCheckedEffectCast29(): (a -> a \ E) = x -> checked_ecast(x)

    @Test
    def testCheckedEffectCast30(): (a -> a) \ E = checked_ecast(x -> x)

}

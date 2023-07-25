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

 mod Test.Exp.UncheckedEffectCast {

    pub eff E

    enum A[a](a)

    def f(x: a): a \ IO = unchecked_cast(x as _ \ IO)

    def g(x: a): a \ E = unchecked_cast(x as _ \ E)

    @test
    def testUncheckedEffectCast01(): Unit \ IO = unchecked_cast(() as _ \ IO)

    @test
    def testUncheckedEffectCast02(): Bool \ IO = unchecked_cast(true as _ \ IO)

    @test
    def testUncheckedEffectCast03(): Bool \ IO = unchecked_cast(not false as _ \ IO)

    @test
    def testUncheckedEffectCast04(): Char \ IO = unchecked_cast('a' as _ \ IO)

    @test
    def testUncheckedEffectCast05(): Char \ IO = unchecked_cast('b' as _ \ IO)

    @test
    def testUncheckedEffectCast06(): Char \ IO = unchecked_cast('Z' as _ \ IO)

    @test
    def testUncheckedEffectCast07(): Char \ IO = unchecked_cast('0' as _ \ IO)

    @test
    def testUncheckedEffectCast08(): Float32 \ IO = unchecked_cast(0.0f32 as _ \ IO)

    @test
    def testUncheckedEffectCast09(): Float32 \ IO = unchecked_cast(123.4f32 as _ \ IO)

    @test
    def testUncheckedEffectCast10(): Float64 \ IO = unchecked_cast(0.0f64 as _ \ IO)

    @test
    def testUncheckedEffectCast11(): Float64 \ IO = unchecked_cast(123.4f64 as _ \ IO)

    @test
    def testUncheckedEffectCast12(): Int8 \ IO = unchecked_cast(0i8 as _ \ IO)

    @test
    def testUncheckedEffectCast13(): Int8 \ IO = unchecked_cast(64i8 as _ \ IO)

    @test
    def testUncheckedEffectCast14(): Int16 \ IO = unchecked_cast(0i16 as _ \ IO)

    @test
    def testUncheckedEffectCast15(): Int16 \ IO = unchecked_cast(64i16 as _ \ IO)

    @test
    def testUncheckedEffectCast16(): Int32 \ IO = unchecked_cast(0i32 as _ \ IO)

    @test
    def testUncheckedEffectCast17(): Int32 \ IO = unchecked_cast(64i32 as _ \ IO)

    @test
    def testUncheckedEffectCast18(): Int64 \ IO = unchecked_cast(0i64 as _ \ IO)

    @test
    def testUncheckedEffectCast19(): Int64 \ IO = unchecked_cast(64i64 as _ \ IO)

    @test
    def testUncheckedEffectCast20(): BigInt \ IO = unchecked_cast(0ii as _ \ IO)

    @test
    def testUncheckedEffectCast21(): BigInt \ IO = unchecked_cast(64ii as _ \ IO)

    @test
    def testUncheckedEffectCast22(): String \ IO = unchecked_cast("ABC" as _ \ IO)

    @test
    def testUncheckedEffectCast23(): Unit \ IO = region rc {
        discard unchecked_cast($ARRAY_NEW$(rc, 'A', 8) as _ \ IO);
        ()
    }

    @test
    def testUncheckedEffectCast24(): Unit \ IO = region rc {
        discard unchecked_cast($ARRAY_NEW$(rc, Some(42), 8) as _ \ IO);
        ()
    }

    @test
    def testUncheckedEffectCast25(): (Char, Int32, Bool) \ IO = unchecked_cast(('a', 42, false) as _ \ IO)

    @test
    def testUncheckedEffectCast26(): A[String] \ IO = unchecked_cast(A.A("ABC") as _ \ IO)

    @test
    def testUncheckedEffectCast27(): (a -> a \ IO) = x -> unchecked_cast(x as _ \ IO)

    @test
    def testUncheckedEffectCast28(): (a -> a) \ IO = unchecked_cast(x -> x as _ \ IO)

    @test
    def testUncheckedEffectCast29(): (a -> a \ E) = x -> unchecked_cast(x as _ \ E)

    @test
    def testUncheckedEffectCast30(): (a -> a) \ E = unchecked_cast(x -> x as _ \ E)

    // Unsafe casts

    @test
    def testUncheckedEffectCast31(): Unit =
        unchecked_cast(f(()) as _ \ {})

    @test
    def testUncheckedEffectCast32(): Bool =
        unchecked_cast(f(true) as _ \ {})

    @test
    def testUncheckedEffectCast33(): Bool =
        unchecked_cast(f(not false) as _ \ {})

    @test
    def testUncheckedEffectCast34(): Char =
        unchecked_cast(f('a') as _ \ {})

    @test
    def testUncheckedEffectCast35(): Float32 =
        unchecked_cast(f(123.4f32) as _ \ {})

    @test
    def testUncheckedEffectCast36(): Float64 =
        unchecked_cast(f(123.4f64) as _ \ {})

    @test
    def testUncheckedEffectCast37(): Int8 =
        unchecked_cast(f(64i8) as _ \ {})

    @test
    def testUncheckedEffectCast38(): Int16 =
        unchecked_cast(f(64i16) as _ \ {})

    @test
    def testUncheckedEffectCast39(): Int32 =
        unchecked_cast(f(64i32) as _ \ {})

    @test
    def testUncheckedEffectCast40(): Int64 =
        unchecked_cast(f(64i64) as _ \ {})

    @test
    def testUncheckedEffectCast41(): BigInt =
        unchecked_cast(f(64ii) as _ \ {})

    @test
    def testUncheckedEffectCast42(): String =
        unchecked_cast(f("ABC") as _ \ {})

    @test
    def testUncheckedEffectCast43(): Unit = region rc {
        let _ = unchecked_cast(f($ARRAY_NEW$(rc, 'A', 8)) as _ \ {});
        ()
    }

    @test
    def testUncheckedEffectCast44(): Unit = region rc {
        let _ = unchecked_cast(f($ARRAY_NEW$(rc, Some(42), 8)) as _ \ {});
        ()
    }

    @test
    def testUncheckedEffectCast45(): (Char, Int32, Bool) = unchecked_cast(f(('a', 42, false)) as _ \ {})

    @test
    def testUncheckedEffectCast46(): A[String] = unchecked_cast(f(A.A("ABC")) as _ \ {})

    @test
    def testUncheckedEffectCast47(): (a -> a) = x -> unchecked_cast(f(x) as _ \ {})

    @test
    def testUncheckedEffectCast48(): (a -> a) = unchecked_cast(f(x -> x) as _ \ {})

    @test
    def testUncheckedEffectCast49(): (a -> a) = x -> unchecked_cast(g(x) as _ \ {})

    @test
    def testUncheckedEffectCast50(): (a -> a) = unchecked_cast(g(x -> x) as _ \ {})

}

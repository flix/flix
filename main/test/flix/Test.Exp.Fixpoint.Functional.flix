/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

mod Test.Exp.Fixpoint.Functional {

    use Assert.assertEq;

    /////////////////////////////////////////////////////////////////////////////
    // Tests with functional predicates                                        //
    /////////////////////////////////////////////////////////////////////////////

    def isPrime(x: Int32): Bool =
        def loop(i) =
            if (i > x / 2) true
            else if (Int32.modulo(x, i) == 0) false
            else loop(i+1);
        if(x < 2) false
        else loop(2)

    def primesInRange(b: Int32, e: Int32): Vector[Int32] =
        Vector.range(b, e) |> Vector.filter(isPrime)

    @Test
    def testFunctional01(): Unit \ Assert = {
        let p = #{
            P(100).
            Primes(x) :- P(y), let x = primesInRange(0, y).
        };
        let res = Vector#{
            2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
            43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
        };
        assertEq(expected = res, query p select (x) from Primes(x))
    }

    @Test
    def testFunctional02(): Unit \ Assert = {
        let startinPrimes = inject Vector#{2, 3, 5, 7, 11} into PrimesUntil/1;
        let p = #{
            PrimesPlus20(x + 20):- PrimesUntil(x).
            PrimesUntil(x) :- PrimesPlus20(y), let x = primesInRange(0, y).
        };
        let res = Vector#{
            2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
            47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103,
            107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163,
            167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227,
            229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
            283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353,
            359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421,
            431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487,
            491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569,
            571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631,
            641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701,
            709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773,
            787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857,
            859, 863, 877, 881, 883, 887
        };
        assertEq(expected = res, query startinPrimes, p select (x) from PrimesUntil(x))
    }

    @Test
    def testFunctional03(): Unit \ Assert = {
        let p = #{
            Start(1).
            End(10).
            Nat(x) :- Start(x).
            Nat(p) :- Succ(x), let p = Vector#{x}.
            Succ(p) :- Nat(x), let p = Vector#{x + 1}, End(y), if(p <= y).
        };
        let res = Vector#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        assertEq(expected = res, query p select x from Nat(x))
    }

    @Test
    def testFunctional04(): Unit \ Assert = {
        let p = #{
            Start(10).
            End(1).
            Nat(x) :- Start(x).
            Nat(p) :- Pre(x), let p = Vector#{x}.
            Pre(p) :- Nat(x), let p = Vector#{x - 1}, End(y), if(p >= y).
        };
        let res = Vector#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        assertEq(expected = res, query p select x from Nat(x))
    }

    @Test
    def testFunctional05(): Unit \ Assert = {
        let p = #{
            Start(0).
            End(10).
            Start2(10).
            End2(20).
            Ranges(p, q) :- Start(x), End(y), let p = Vector.range(x, y), Start2(a), End2(b), let q = Vector.range(a, b).
        };
        let res = Vector.range(0, 10) |> Vector.flatMap(x -> Vector.range(10, 20) |> Vector.map(y -> (x, y))) |> Vector.sort;
        assertEq(expected = res, query p select (x, y) from Ranges(x, y))
    }

    @Test
    def testFunctionalZero01(): Unit \ Assert = {
        let p = #{
            Empty(p) :- let p = Vector#{}.
        };
        let res: Vector[Int32] = Vector#{};
        assertEq(expected = res, query p select x from Empty(x))
    }

    @Test
    def testFunctionalZero02(): Unit \ Assert = {
        let p = #{
            NonEmpty(p) :- let p = Vector#{1}.
        };
        assertEq(expected = Vector#{1}, query p select x from NonEmpty(x))
    }

    @Test
    def testFunctionalZero03(): Unit \ Assert = {
        let p = #{
            Range(p) :- let p = Vector.range(0, 10).
        };
        assertEq(expected = Vector.range(0, 10), query p select x from Range(x))
    }

    @Test
    def testFunctionalZero04(): Unit \ Assert = {
        let p = #{
            Primes(x) :- let x = primesInRange(0, 100).
        };
        let res = Vector#{
            2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
            43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
        };
        assertEq(expected = res, query p select (x) from Primes(x))
    }

    @Test
    def testFunctionalZero05(): Unit \ Assert = {
        let p = #{
            NonEmpty(x) :- let x = Vector#{"SomeString"}.
        };
        assertEq(expected = Vector#{"SomeString"}, query p select (x) from NonEmpty(x))
    }

    @Test
    def testFunctionalZero06(): Unit \ Assert = {
        let p = #{
            NonEmpty(x) :- let x = Vector#{(1, 2)}.
        };
        assertEq(expected = Vector#{(1, 2)}, query p select (x) from NonEmpty(x))
    }

    @Test
    def testFunctionalZero07(): Unit \ Assert = {
        let p = #{
            NonEmpty(x) :- let x = Vector#{1i64}.
        };
        assertEq(expected = Vector#{1i64}, query p select (x) from NonEmpty(x))
    }

    @Test
    def testFunctionalZero08(): Unit \ Assert = {
        let p = #{
            NonEmpty(x) :- let x = Vector#{1f64}.
        };
        assertEq(expected = Vector#{1f64}, query p select (x) from NonEmpty(x))
    }

    @Test
    def testFunctionalUnusedVar01(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(x) :- A(x), let y = Vector#{3}, if (y + x > 5 and x + y < 7).
        };
        assertEq(expected = Vector#{3}, query db, pr select x from R(x))
    }

    @Test
    def testFunctionalUnusedVar02(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(x) :- A(x), let y = Vector#{3}.
        };
        assertEq(expected = Vector#{1, 3, 5}, query db, pr select x from R(x))
    }

    @Test
    def testFunctionalUnusedVar03(): Unit \ Assert = {
        let db = #{
            A(1). A(3). A(5).
        };
        let pr = #{
            R(10) :- A(x), let y = Vector#{x}.
        };
        assertEq(expected = Vector#{10}, query db, pr select x from R(x))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with lattice values                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testFunctional06(): Unit \ Assert = {
        let p = #{
            Start(;0).
            End(;10).
            InRange(p) :- let p = Vector.range(x, y), Start(;x), End(;y).
        };
        assertEq(expected = Vector.range(0, 10), query p select x from InRange(x))
    }

    def vectorRangeFromDown(x: Down[Int32], y: Down[Int32]): Vector[Int32] = {
        let Down.Down(a) = x;
        let Down.Down(b) = y;
        Vector.range(a, b)
    }

    @Test
    def testFunctional07(): Unit \ Assert = {
        let p = #{
            Start(;Down.Down(0)).
            Mid(;Down.Down(3)).
            End(;Down.Down(10)).
            InRange(p) :- let p = vectorRangeFromDown(x, y), Start(;x), Mid(;x), End(;y).
        };
        assertEq(expected = Vector.range(3, 10), query p select x from InRange(x))
    }

    @Test
    def testFunctional08(): Unit \ Assert = {
        let p = #{
            Start(;Down.Down(0)).
            Mid(;Down.Down(3)).
            End(;Down.Down(10)).
            InRange(p) :- let p = vectorRangeFromDown(x, y), Mid(;x), Start(;x), End(;y).
        };
        assertEq(expected = Vector.range(3, 10), query p select x from InRange(x))
    }
}

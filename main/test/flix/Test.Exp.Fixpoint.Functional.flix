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
    def testFunctional01(): Bool = {
        let p = #{
            P(100).
            Primes(x) :- P(y), let x = primesInRange(0, y).
        };
        let res = Vector#{
            2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
            43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
        };
        res `Assert.eq` query p select (x) from Primes(x)
    }

    @Test
    def testFunctional02(): Bool = {
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
        res `Assert.eq` query startinPrimes, p select (x) from PrimesUntil(x)
    }

    @Test
    def testFunctional03(): Bool = {
        let p = #{
            Start(1).
            End(10).
            Nat(x) :- Start(x).
            Nat(p) :- Succ(x), let p = Vector#{x}.
            Succ(p) :- Nat(x), let p = Vector#{x + 1}, End(y), if(p <= y).
        };
        let res = Vector#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        res `Assert.eq` query p select x from Nat(x)
    }

    @Test
    def testFunctional04(): Bool = {
        let p = #{
            Start(10).
            End(1).
            Nat(x) :- Start(x).
            Nat(p) :- Pre(x), let p = Vector#{x}.
            Pre(p) :- Nat(x), let p = Vector#{x - 1}, End(y), if(p >= y).
        };
        let res = Vector#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        res `Assert.eq` query p select x from Nat(x)
    }

    @Test
    def testFunctional05(): Bool = {
        let p = #{
            Start(0).
            End(10).
            Start2(10).
            End2(20).
            Ranges(p, q) :- Start(x), End(y), let p = Vector.range(x, y), Start2(a), End2(b), let q = Vector.range(a, b).
        };
        let res = Vector.range(0, 10) |> Vector.flatMap(x -> Vector.range(10, 20) |> Vector.map(y -> (x, y))) |> Vector.sort;
        res `Assert.eq` query p select (x, y) from Ranges(x, y)
    }
}

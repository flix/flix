/*
 * Copyright 2019 Magnus Madsen
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

namespace Random {

    ///
    /// Returns a fresh random number generator.
    ///
    pub def new(): Random[static] & static =
        import new java.util.Random(): ##java.util.Random & Impure as newRandom;
        Random(newRandom())

    ///
    /// Returns a fresh random number generator initialized with the given seed `s`.
    ///
    pub def newWithSeed(_r: Region[r], s: Int64): Random[r] & r =
        import new java.util.Random(Int64): ##java.util.Random & r as newRandom;
        Random(newRandom(s))

    ///
    /// Returns a pseudorandom element from the given array `a` using the random number generator `r`.
    ///
    /// Returns `None` if the given array `a` is empty.
    ///
    pub def choose(ran: Random[r1], a: ScopedArray[a, r2]): Option[a] & r1 and r2 =
        if (a.length == 0) {
            None
        } else {
            let m = a.length;
            let i = nextNatWithMax(ran, m);
            Some(a[i])
        }

    ///
    /// Shuffles `a` to a random permutation.
    /// OBS not acutally uniform :L
    ///
    pub def shuffleArray(ran: Random[r1], a: ScopedArray[a, r2]): Unit & r1 and r2 =
        let len = Array.length(a);
        def swapIndexRandomly(e: a, i: Int32) = {
            let randomIndex = nextNatWithMax(ran, len);
            a[i] = a[randomIndex];
            a[randomIndex] = e
        };
        Array.foreachWithIndex(swapIndexRandomly, a)

    ///
    /// Returns a random permutation of `l`.
    /// OBS not acutally uniform :L
    ///
    pub def shuffleList(ran: Random[r], l: List[a]): List[a] & r =
        region r1 {
            let arr = listToArray(l, r1);
            shuffleArray(ran, arr);
            arrayToList(arr)
        }

    ///
    /// Returns the next pseudorandom boolean from the given random number generator `r`.
    ///
    pub def nextBool(ran: Random[r]): Bool & r =
        import java.util.Random.nextBoolean(): Bool & r;
        let Random(o) = ran;
        nextBoolean(o)

    ///
    /// Returns the next pseudorandom 32-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat32(ran: Random[r]): Float32 & r =
        import java.util.Random.nextFloat(): Float32 & r;
        let Random(o) = ran;
        nextFloat(o)

    ///
    /// Returns the next pseudorandom 64-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat64(ran: Random[r]): Float64 & r =
        import java.util.Random.nextDouble(): Float64 & r;
        let Random(o) = ran;
        nextDouble(o)

    ///
    /// Returns the next pseudorandom 32-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt32(ran: Random[r]): Int32 & r =
        import java.util.Random.nextInt(): Int32 & r;
        let Random(o) = ran;
        nextInt(o)

    ///
    /// Returns the next pseudorandom 64-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt64(ran: Random[r]): Int64 & r =
        import java.util.Random.nextLong(): Int64 & r;
        let Random(o) = ran;
        nextLong(o)

    ///
    /// Returns the next pseudorandom Gaussian distributed 64-bit floating point number.
    ///
    pub def nextGaussian(ran: Random[r]): Float64 & r =
        import java.util.Random.nextGaussian(): Float64 & r;
        let Random(o) = ran;
        nextGaussian(o)

    ///
    /// Returns the next pseudorandom 32-bit integer value between `0` and `m` (exclusive) using the given random number generator `r`.
    ///
    pub def nextNatWithMax(ran: Random[r], m: Int32): Int32 & r =
        import java.util.Random.nextInt(Int32): Int32 & r;
        let Random(o) = ran;
        nextInt(o, m)

    ///
    /// Represents a random number generator.
    ///
    pub enum Random[_r: Region] {
        case Random(##java.util.Random)
    }

    ///
    /// Converts a list to an array.
    /// OBS: This should be implemented in List.flix at some point.
    ///
    def listToArray(l: List[a], r: Region[r]): ScopedArray[a, r] & r = {
        use List.{length, zipWithIndex, foreach};
        match l {
            case Nil => [] @ r
            case hd :: _ =>
                let arr = [hd; length(l)] @ r;
                l |> zipWithIndex |> foreach(match (e, i) -> arr[i] = e);
                arr
        }
    }

    ///
    /// Converts an array to a list.
    /// OBS: This should be implemented in Array.flix at some point.
    ///
    def arrayToList(a: ScopedArray[e, r]): List[e] & r =
        List.range(0, Array.length(a)) |> List.map(i -> a[i])

}

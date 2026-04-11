/*
 * Copyright 2026 Magnus Madsen
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

///
/// Portable-only: default handler implementation for `Random`.
///
/// The seeded API (`handleWithSeed` / `runWithSeed`) is implemented as a pure Flix PRNG
/// matching the `java.util.Random` LCG for `Bool`, `Int32`, `Int64`, `Float32`, and `Float64`.
///
/// Note: `randomGaussian` currently uses a deterministic CLT approximation (sum of 12 uniforms - 6).
/// This preserves portability and determinism, but does not match `java.util.Random#nextGaussian`.
///

pub mod Random {

    ///
    /// Handles the `Random` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Random` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Random) + IO =
        x -> {
            let seed = Int64.bitwiseXor(%%NEW_ID%%(()), %%TIME_NOW_MILLIS%%(()));
            handleWithSeed(seed, f)(x)
        }

    ///
    /// Runs the `Random` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Random` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Random) + IO = handle(f)()

    ///
    /// Runs the `Random` effect of the given function `f` from an initial seed.
    ///
    /// In other words, re-interprets the `Random` effect using seeded deterministic randomness.
    ///
    pub def handleWithSeed(seed: Int64, f: a -> b \ ef): a -> b \ ef - Random =
        x -> region rc {
            let seedRef = Ref.fresh(rc, initialSeed(seed));
            run {
                f(x)
            } with handler Random {
                def randomBool(k)       = k(nextBool(seedRef))
                def randomFloat32(k)    = k(nextFloat32(seedRef))
                def randomFloat64(k)    = k(nextFloat64(seedRef))
                def randomInt32(k)      = k(nextInt32(seedRef))
                def randomInt64(k)      = k(nextInt64(seedRef))
                def randomGaussian(k)   = k(nextGaussian(seedRef))
            }
        }

    ///
    /// Runs the `Random` effect of the given function `f` from an initial seed.
    ///
    /// In other words, re-interprets the `Random` effect using seeded deterministic randomness.
    ///
    pub def runWithSeed(seed: Int64, f: Unit -> a \ ef): a \ ef - Random = handleWithSeed(seed, f)()

    // ------------------------------------------------------------------------
    // Java `Random` LCG implementation.
    // ------------------------------------------------------------------------

    def lcgMultiplier(): Int64 = 0x5DEECE66Di64

    def lcgAddend(): Int64 = 0xBi64

    def lcgMask(): Int64 = 0xFFFFFFFFFFFFi64

    def initialSeed(seed: Int64): Int64 =
        Int64.bitwiseAnd(Int64.bitwiseXor(seed, lcgMultiplier()), lcgMask())

    def next(bits: Int32, seedRef: Ref[Int64, r]): Int32 \ r = {
        let oldSeed = Ref.get(seedRef);
        let newSeed = Int64.bitwiseAnd(oldSeed * lcgMultiplier() + lcgAddend(), lcgMask());
        Ref.put(newSeed, seedRef);

        let shift = 48 - bits;
        let x = Int64.logicalRightShift(dist = shift, newSeed);

        if (bits == 32) u32ToInt32Bits(x) else i64ToInt32Exact(x)
    }

    ///
    /// Converts `x` in the range [0, 2^32) to an `Int32` by interpreting the low 32 bits
    /// as a signed two's complement integer.
    ///
    def u32ToInt32Bits(x: Int64): Int32 =
        let signed = if (x >= 0x8000_0000i64) x - 0x1_0000_0000i64 else x;
        i64ToInt32Exact(signed)

    ///
    /// Converts `x` to `Int32` assuming it is already within range.
    ///
    def i64ToInt32Exact(x: Int64): Int32 = match Int64.tryToInt32(x) {
        case Some(v) => v
        case None    => 0 // unreachable
    }

    def nextBool(seedRef: Ref[Int64, r]): Bool \ r = next(1, seedRef) != 0

    def nextInt32(seedRef: Ref[Int64, r]): Int32 \ r = next(32, seedRef)

    def nextInt64(seedRef: Ref[Int64, r]): Int64 \ r = {
        let hi = Int32.toInt64(next(32, seedRef));
        let lo = Int32.toInt64(next(32, seedRef));
        Int64.leftShift(hi, 32) + lo
    }

    def nextFloat32(seedRef: Ref[Int64, r]): Float32 \ r =
        Int32.toFloat32(next(24, seedRef)) / 16_777_216.0f32

    def nextFloat64(seedRef: Ref[Int64, r]): Float64 \ r = {
        let a = Int32.toInt64(next(26, seedRef));
        let b = Int32.toInt64(next(27, seedRef));
        let x = Int64.leftShift(a, 27) + b;
        Int64.toFloat64(x) / 9_007_199_254_740_992.0f64
    }

    ///
    /// Deterministic CLT approximation of a standard normal random variable.
    ///
    def nextGaussian(seedRef: Ref[Int64, r]): Float64 \ r =
        sumUniform(12, 0.0f64, seedRef) - 6.0f64

    def sumUniform(n: Int32, acc: Float64, seedRef: Ref[Int64, r]): Float64 \ r =
        if (n <= 0) acc else sumUniform(n - 1, acc + nextFloat64(seedRef), seedRef)

}

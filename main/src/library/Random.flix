namespace Random {

    ///
    /// Returns a fresh random number generator.
    ///
    pub def new(): Random & Impure =
        import new java.util.Random() as newRandom;
        Random(newRandom())

    ///
    /// Returns a fresh random number generator initialized with the given seed `s`.
    ///
    pub def newWithSeed(s: Int64): Random & Impure =
        import new java.util.Random(Int64) as newRandom;
        Random(newRandom(s))

    ///
    /// Returns a pseudorandom element from the given array `a` using the random number generator `r`.
    ///
    /// Returns `None` if the given array `a` is empty.
    ///
    pub def choose(r: Random, a: Array[a]): Option[a] & Impure =
        if (a.length == 0) {
            None
        } else {
            let m = a.length;
            let i = nextNatWithMax(r, m);
            Some(a[i])
        }

    ///
    /// Returns the next pseudorandom boolean from the given random number generator `r`.
    ///
    pub def nextBool(r: Random): Bool & Impure =
        import java.util.Random.nextBoolean();
        let Random(o) = r;
        o.nextBoolean()

    ///
    /// Returns the next pseudorandom 32-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat32(r: Random): Float32 & Impure =
        import java.util.Random.nextFloat();
        let Random(o) = r;
        o.nextFloat()

    ///
    /// Returns the next pseudorandom 64-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat64(r: Random): Float64 & Impure =
        import java.util.Random.nextDouble();
        let Random(o) = r;
        o.nextDouble()

    ///
    /// Returns the next pseudorandom 32-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt32(r: Random): Int32 & Impure =
        import java.util.Random.nextInt();
        let Random(o) = r;
        o.nextInt()

    ///
    /// Returns the next pseudorandom 64-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt64(r: Random): Int64 & Impure =
        import java.util.Random.nextLong();
        let Random(o) = r;
        o.nextLong()

    ///
    /// Returns the next pseudorandom Gaussian distributed 64-bit floating point number.
    ///
    pub def nextGaussian(r: Random): Float64 & Impure =
        import java.util.Random.nextGaussian();
        let Random(o) = r;
        o.nextGaussian()

    ///
    /// Returns the next pseudorandom 32-bit integer value between `0` and `m` (exclusive) using the given random number generator `r`.
    ///
    pub def nextNatWithMax(r: Random, m: Int32): Int32 & Impure =
        import java.util.Random.nextInt(Int32);
        let Random(o) = r;
        o.nextInt(m)

    ///
    /// Represents a random number generator.
    ///
    opaque type Random = ##java.util.Random

}

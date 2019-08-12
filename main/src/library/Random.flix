namespace Random {

    ///
    /// Returns a fresh random number generator.
    ///
    pub def new(): Random = Random(native new java.util.Random())

    ///
    /// Returns a fresh random number generator initialized with the given `seed`.
    ///
    pub def newWithSeed(seed: Int64): Random = Random(native new java.util.Random(seed))

    ///
    /// Returns a random element from the given array `a` using the given random number generator `r`.
    ///
    pub def choose[a](r: Random, a: Array[a]): a =
        let m = length[a];
        let i = nextInt32WithMax(r, m);
        a[i]

    ///
    /// Returns the next pseudorandom boolean from the given random number generator `r`.
    ///
    pub def nextBool(r: Random): Bool =
        let Random(o) = r;
        native method java.util.Random.nextBoolean(o) as Bool

    ///
    /// Returns the next pseudorandom 32-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat32(r: Random): Float32 =
        let Random(o) = r;
        native method java.util.Random.nextFloat(o) as Float32

    ///
    /// Returns the next pseudorandom 64-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat64(r: Random): Float64 =
        let Random(o) = r;
        native method java.util.Random.nextDouble(o) as Float64

    ///
    /// Returns the next pseudorandom 32-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt32(r: Random): Int32 =
        let Random(o) = r;
        native method java.util.Random.nextInt(o) as Int32

    ///
    /// Returns the next pseudorandom 32-bit integer value between `0` and `m` (exclusive) using the given random number generator `r`.
    ///
    pub def nextInt32WithMax(r: Random, m: Int32): Int32 =
        let Random(o) = r;
        native method java.util.Random.nextInt(o, m) as Int32

    ///
    /// Returns the next pseudorandom 64-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt64(r: Random): Int64 =
        let Random(o) = r;
        native method java.util.Random.nextLong(o) as Int64

    ///
    /// Returns the next pseudorandom Gaussian distributed 64-bit floating point number.
    ///
    pub def nextGaussian(r: Random): Float64 =
        let Random(o) = r;
        native method java.util.Random.nextGaussian(o) as Float64

    ///
    /// Represents a random number generator.
    ///
    pub type Random = Random(##java.util.Random)

}

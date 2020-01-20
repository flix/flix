namespace Random {

    ///
    /// Returns a fresh random number generator.
    ///
    pub def new(): Random = Random(native new java.util.Random())

    ///
    /// Returns a fresh random number generator initialized with the given seed `s`.
    ///
    pub def newWithSeed(s: Int64): Random = Random(native new java.util.Random(s))

    ///
    /// Returns a pseudorandom element from the given array `a` using the random number generator `r`.
    ///
    /// Returns `None` if the given array `a` is empty.
    ///
    pub def choose[a](r: Random, a: Array[a]): Option[a] =
        if (length[a] == 0) {
            None
        } else {
            let m = length[a];
            let i = nextNatWithMax(r, m);
            Some(a[i])
        }

    ///
    /// Returns the next pseudorandom boolean from the given random number generator `r`.
    ///
    pub def nextBool(r: Random): Bool =
        import java.util.Random.nextBoolean();
        let Random(o) = r;
        o.nextBoolean()

    ///
    /// Returns the next pseudorandom 32-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat32(r: Random): Float32 =
        import java.util.Random.nextFloat();
        let Random(o) = r;
        o.nextFloat()

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
    /// Returns the next pseudorandom 32-bit integer value between `0` and `m` (exclusive) using the given random number generator `r`.
    ///
    pub def nextNatWithMax(r: Random, m: Int32): Int32 =
        let Random(o) = r;
        native method java.util.Random.nextInt(o, m) as Int32

    ///
    /// Represents a random number generator.
    ///
    opaque type Random = ##java.util.Random

}

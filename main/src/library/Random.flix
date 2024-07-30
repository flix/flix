
///
/// Represents a random number generator.
///
pub enum Random(##java.util.Random)

mod Random {
    import java.util.{Random => JRandom};

    ///
    /// Returns a fresh random number generator.
    ///
    pub def fresh(): Random \ IO =
        Random(new JRandom())

    ///
    /// Returns a fresh random number generator initialized with the given seed `s`.
    ///
    pub def freshWithSeed(s: Int64): Random \ IO =
        Random(new JRandom(s))

    ///
    /// Returns a pseudorandom element from the given array `a` using the random number generator `r`.
    ///
    /// Returns `None` if the given array `a` is empty.
    ///
    pub def oneOf(r: Random, a: Array[a, r]): Option[a] \ { IO, r } =
        if (Array.length(a) == 0) {
            None
        } else {
            let m = Array.length(a);
            let i = nextNatWithMax(r, m);
            Some(Array.get(i, a))
        }

    ///
    /// Returns the next pseudorandom boolean from the given random number generator `r`.
    ///
    pub def nextBool(r: Random): Bool \ IO =
        let Random(o) = r;
        o.nextBoolean()

    ///
    /// Returns the next pseudorandom 32-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat32(r: Random): Float32 \ IO =
        let Random(o) = r;
        o.nextFloat()

    ///
    /// Returns the next pseudorandom 64-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat64(r: Random): Float64 \ IO =
        let Random(o) = r;
        o.nextDouble()

    ///
    /// Returns the next pseudorandom 32-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt32(r: Random): Int32 \ IO =
        let Random(o) = r;
        o.nextInt()

    ///
    /// Returns the next pseudorandom 64-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt64(r: Random): Int64 \ IO =
        let Random(o) = r;
        o.nextLong()

    ///
    /// Returns the next pseudorandom Gaussian distributed 64-bit floating point number.
    ///
    pub def nextGaussian(r: Random): Float64 \ IO =
        let Random(o) = r;
        o.nextGaussian()

    ///
    /// Returns the next pseudorandom 32-bit integer value between `0` and `m` (exclusive) using the given random number generator `r`.
    ///
    pub def nextNatWithMax(r: Random, m: Int32): Int32 \ IO =
        let Random(o) = r;
        o.nextInt(m)
}

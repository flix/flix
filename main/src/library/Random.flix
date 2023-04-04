
///
/// Represents a random number generator.
///
pub enum Random(##java.util.Random)

mod Random {

    ///
    /// Returns a fresh random number generator.
    ///
    pub def new(): Random \ NonDet =
        import new java.util.Random(): ##java.util.Random \ NonDet as newRandom;
        Random(newRandom())

    ///
    /// Returns a fresh random number generator initialized with the given seed `s`.
    ///
    pub def newWithSeed(s: Int64): Random \ NonDet =
        import new java.util.Random(Int64): ##java.util.Random \ NonDet as newRandom;
        Random(newRandom(s))

    ///
    /// Returns a pseudorandom element from the given array `a` using the random number generator `r`.
    ///
    /// Returns `None` if the given array `a` is empty.
    ///
    pub def choose(r: Random, a: Array[a, r]): Option[a] \ { NonDet, r } =
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
    pub def nextBool(r: Random): Bool \ NonDet =
        import java.util.Random.nextBoolean(): Bool \ NonDet;
        let Random(o) = r;
        nextBoolean(o)

    ///
    /// Returns the next pseudorandom 32-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat32(r: Random): Float32 \ NonDet =
        import java.util.Random.nextFloat(): Float32 \ NonDet;
        let Random(o) = r;
        nextFloat(o)

    ///
    /// Returns the next pseudorandom 64-bit floating point number from the given random number generator `r`.
    ///
    pub def nextFloat64(r: Random): Float64 \ NonDet =
        import java.util.Random.nextDouble(): Float64 \ NonDet;
        let Random(o) = r;
        nextDouble(o)

    ///
    /// Returns the next pseudorandom 32-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt32(r: Random): Int32 \ NonDet =
        import java.util.Random.nextInt(): Int32 \ NonDet;
        let Random(o) = r;
        nextInt(o)

    ///
    /// Returns the next pseudorandom 64-bit integer value from the given random number generator `r`.
    ///
    pub def nextInt64(r: Random): Int64 \ NonDet =
        import java.util.Random.nextLong(): Int64 \ NonDet;
        let Random(o) = r;
        nextLong(o)

    ///
    /// Returns the next pseudorandom Gaussian distributed 64-bit floating point number.
    ///
    pub def nextGaussian(r: Random): Float64 \ NonDet =
        import java.util.Random.nextGaussian(): Float64 \ NonDet;
        let Random(o) = r;
        nextGaussian(o)

    ///
    /// Returns the next pseudorandom 32-bit integer value between `0` and `m` (exclusive) using the given random number generator `r`.
    ///
    pub def nextNatWithMax(r: Random, m: Int32): Int32 \ NonDet =
        import java.util.Random.nextInt(Int32): Int32 \ NonDet;
        let Random(o) = r;
        nextInt(o, m)
}


/// Represents a splittable pseudorandom number.
pub enum SplittableRandom(##java.util.SplittableRandom)

mod SplittableRandom {

    /// Returns a new SplittableRandom instance that is initialized with a random seed.
    pub def fresh(): SplittableRandom \ IO =
        import java_new java.util.SplittableRandom(): ##java.util.SplittableRandom \ IO as newSplittableRandom;
        SplittableRandom(newSplittableRandom())

    /// Returns a new SplittableRandom instance that is initialized with the specified seed.
    pub def freshWithSeed(seed: Int64): SplittableRandom \ IO =
        import java_new java.util.SplittableRandom(Int64): ##java.util.SplittableRandom \ IO as newSplittableRandom;
        SplittableRandom(newSplittableRandom(seed))

    /// Returns a new SplittableRandom instance that is independent of the current instance.
    pub def split(sr: SplittableRandom): (SplittableRandom, SplittableRandom) \ IO =
        import java.util.SplittableRandom.split(): ##java.util.SplittableRandom \ IO;
        let SplittableRandom(object) = sr;
        (sr, SplittableRandom(split(object)))

    /// Returns a pseudorandom boolean value.
    pub def nextBool(sr: SplittableRandom): Bool \ IO =
        import java.util.SplittableRandom.nextBoolean(): Bool \ IO;
        let SplittableRandom(object) = sr;
        nextBoolean(object)

    /// Returns a pseudorandom int32 value between the specified origin (inclusive) and the specified bound (exclusive).
    pub def nextInt32(sr: SplittableRandom, origin: Int32, bound: Int32): Int32 \ IO =
        import java.util.SplittableRandom.nextInt(Int32, Int32): Int32 \ IO;
        let SplittableRandom(object) = sr;
        nextInt(object, origin, bound)

    /// Returns a pseudorandom Int64 value between the specified origin (inclusive) and the specified bound (exclusive).
    pub def nextInt64(sr: SplittableRandom, origin: Int64, bound: Int64): Int64 \ IO =
        import java.util.SplittableRandom.nextLong(Int64, Int64): Int64 \ IO;
        let SplittableRandom(object) = sr;
        nextLong(object, origin, bound)

    /// Returns a pseudorandom Float64 value between the specified origin (inclusive) and the specified bound (exclusive).
    pub def nextFloat64(sr: SplittableRandom, origin: Float64, bound: Float64): Float64 \ IO =
        import java.util.SplittableRandom.nextDouble(Float64, Float64): Float64 \ IO;
        let SplittableRandom(object) = sr;
        nextDouble(object, origin, bound)

}
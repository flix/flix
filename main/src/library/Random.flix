namespace Random {

    ///
    /// TODO
    ///
    pub type Random = Random(##java.util.Random)

    ///
    /// TODO
    ///
    pub def new(): Random = Random(native new java.util.Random())

    ///
    /// TODO
    ///
    pub def nextBool(r: Random): Bool =
        let Random(o) = r;
        native method java.util.Random.nextBoolean(o) as Bool

    ///
    /// TODO
    ///
    pub def nextFloat64(r: Random): Float64 =
        let Random(o) = r;
        native method java.util.Random.nextDouble(o) as Float64

}

//Returns the next pseudorandom, uniformly distributed double value between 0.0 and 1.0 from this random number generator's sequence.
//float	nextFloat()

//Returns the next pseudorandom, uniformly distributed float value between 0.0 and 1.0 from this random number generator's sequence.
//double	nextGaussian()

//Returns the next pseudorandom, Gaussian ("normally") distributed double value with mean 0.0 and standard deviation 1.0 from this random number generator's sequence.
//int	nextInt()

//Returns the next pseudorandom, uniformly distributed int value from this random number generator's sequence.
//int	nextInt​(int bound)

//Returns a pseudorandom, uniformly distributed int value between 0 (inclusive) and the specified value (exclusive), drawn from this random number generator's sequence.
//long	nextLong()

//Returns the next pseudorandom, uniformly distributed long value from this random number generator's sequence.

//void	setSeed​(long seed)
//Sets the seed of this random number generator using a single long seed.

//
//def nextInt(n: Int): Int
//
////Returns a pseudorandom, uniformly distributed int value between 0 (inclusive) and the specified value (exclusive), drawn from this random number generator's sequence.
//
//def between(minInclusive: Long, maxExclusive: Long): Long
//
//Returns a pseudorandom, uniformly distributed long value between min (inclusive) and the specified value max (exclusive), drawn from this random number generator's sequence.
//def between(minInclusive: Int, maxExclusive: Int): Int
//
//Returns a pseudorandom, uniformly distributed int value between min (inclusive) and the specified value max (exclusive), drawn from this random number generator's sequence.
//def between(minInclusive: Float, maxExclusive: Float): Float
//
//Returns the next pseudorandom, uniformly distributed float value between min (inclusive) and max (exclusive) from this random number generator's sequence.
//def between(minInclusive: Double, maxExclusive: Double): Double
//
//Returns the next pseudorandom, uniformly distributed double value between min (inclusive) and max (exclusive) from this random number generator's sequence.
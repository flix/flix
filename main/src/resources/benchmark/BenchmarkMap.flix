mod BenchmarkMap {

    ///
    /// create a map of `Int32 -> Int32` and compute the size of it.
    ///
    @Benchmark
    pub def benchmark01(): Bool =
        Map.size(range(0, 24)) == 24

    ///
    /// create a map of `Int32 -> Int32` and get value.
    ///
    @Benchmark
    pub def benchmark02(): Bool =
        Map.get(12, range(0, 12)) == Some(12)

    ///
    /// create a map of `Int32 -> Int32` and check if it contains a low-valued mapping.
    ///
    @Benchmark
    pub def benchmark03(): Bool =
        Map.memberOf(0, range(0, 24))

    ///
    /// create a map of `Int32 -> Int32` and check if it contains a mid-valued mapping.
    ///
    @Benchmark
    pub def benchmark04(): Bool =
        Map.memberOf(12, range(0, 24))

    ///
    /// create a map of `Int32 -> Int32` and check if it contains a high-valued mapping.
    ///
    @Benchmark
    pub def benchmark05(): Bool =
        Map.memberOf(23, range(0, 24))

    ///
    /// create a map of `Int32 -> Int32` and check if it contains a non-existent mapping.
    ///
    @Benchmark
    pub def benchmark06(): Bool =
        not Map.memberOf(24, range(0, 24))

    ///
    /// create a map of `Int32 -> Int32` and compute its keys.
    ///
    @Benchmark
    pub def benchmark07(): Bool =
        Set.size(Map.keysOf(range(0, 24))) == 24

    ///
    /// create a map of `Int32 -> Int32` and compute its values.
    ///
    @Benchmark
    pub def benchmark08(): Bool =
        List.length(Map.valuesOf(range(0, 24))) == 24

    ///
    /// create a map of `Int32 -> Int32`, delete the lower half of the mappings and compute the size.
    ///
    @Benchmark
    pub def benchmark09(): Bool =
        Map.size(Map.foldLeftWithKey((acc, k, _) -> Map.remove(k, acc), range(0, 24), range(0, 12))) == 12

    ///
    /// create a map of `Int32 -> Int32`, delete the upper half of the mappings and compute the size.
    ///
    @Benchmark
    pub def benchmark10(): Bool =
        Map.size(Map.foldLeftWithKey((acc, k, _) -> Map.remove(k, acc), range(0, 24), range(12, 24))) == 12

    ///
    /// create two maps of `Int32 -> Int32`, find their difference and compute the size.
    ///
    @Benchmark
    pub def benchmark11(): Bool =
        Map.size(Map.difference(range(0, 12), range(12, 24))) == 12

    ///
    /// create a map of `Int32 -> Int32` and check if at least one mapping satisfies a predicate.
    ///
    @Benchmark
    pub def benchmark12(): Bool =
        Map.exists((k, v) -> k == 12 and v == 12, range(0, 24))

    ///
    /// create a map of `Int32 -> Int32` and check if at least one mapping satisfies a predicate.
    ///
    @Benchmark
    pub def benchmark13(): Bool =
        Map.exists((k, v) -> k > 22 and v > 22, range(0, 24))

    ///
    /// create a map of `Int32 -> Int32` and check if at least one mapping satisfies a predicate.
    ///
    @Benchmark
    pub def benchmark14(): Bool =
        not Map.exists((k, v) -> k > 23 and v > 23, range(0, 24))

    ///
    /// create a map of `Int32 -> Int32`, filter out the mappings with even keys and compute the size.
    ///
    @Benchmark
    pub def benchmark15(): Bool =
        Map.size(Map.filterWithKey((k, _) -> Int32.rem(k, 2) == 0, range(0, 24))) == 12

    ///
    /// create a map of `Int32 -> Int32` and find the first mapping that satisfies a predicate, searching left to right.
    ///
    @Benchmark
    pub def benchmark16(): Bool =
        Map.findLeft((k, _) -> k == 23, range(0, 24)) == Some((23, 23))

    ///
    /// create a map of `Int32 -> Int32` and find the first mapping that satisfies a predicate, searching right to left.
    ///
    @Benchmark
    pub def benchmark17(): Bool =
        Map.findRight((k, _) -> k == 0, range(0, 24)) == Some((0, 0))

    ///
    /// create a map of `Int32 -> Int32` and compute the sum of all values via foldLeft.
    ///
    @Benchmark
    pub def benchmark18(): Bool =
        Map.foldLeft((acc, v) -> acc + v, 0, range(0, 24)) == 276

    ///
    /// create a map of `Int32 -> Int32` and compute the sum of all values via foldRight.
    ///
    @Benchmark
    pub def benchmark19(): Bool =
        Map.foldRight((acc, v) -> acc + v, 0, range(0, 24)) == 276

    ///
    /// create a map of `Int32 -> Int32` and check if all mappings satisfy a predicate.
    ///
    @Benchmark
    pub def benchmark20(): Bool =
        Map.forAll((k, v) -> k < 24 and v < 24, range(0, 24))

    ///
    /// create two maps of `Int32 -> Int32`, find their intersection and compute the size.
    ///
    @Benchmark
    pub def benchmark21(): Bool =
        Map.size(Map.intersection(range(0, 12), range(0, 24))) == 12

    ///
    /// create two maps of `Int32 -> Int32` and check if the first is a submap of the second.
    ///
    @Benchmark
    pub def benchmark22(): Bool =
        Map.isSubmapOf(range(0, 24), range(0, 24))

    ///
    /// create two maps of `Int32 -> Int32` and check if the first is a proper submap of the second.
    ///
    @Benchmark
    pub def benchmark23(): Bool =
        Map.isSubmapOf(range(0, 12), range(0, 24))

    ///
    /// create a map of `Int32 -> Int32`, increment each value by one and compute the size.
    ///
    @Benchmark
    pub def benchmark24(): Bool =
        Map.size(Map.map(v -> v + 1, range(0, 24))) == 24

    ///
    /// create a map of `Int32 -> Int32` and compute the sum of its values via reduceLeft.
    ///
    @Benchmark
    pub def benchmark25(): Bool =
        Map.reduceLeft((x, y) -> x + y, range(0, 24)) == Some(276)

    ///
    /// create a map of `Int32 -> Int32` and compute the sum of its values via reduceRight.
    ///
    @Benchmark
    pub def benchmark26(): Bool =
        Map.reduceRight((x, y) -> x + y, range(0, 24)) == Some(276)

    ///
    /// create a map of `Int32 -> Int32` and update the value of a low-valued key.
    ///
    @Benchmark
    pub def benchmark27(): Bool =
        Map.exists((_, v) -> v == 24, Map.adjust(_ -> 42, 0, range(0, 24)))

    ///
    /// create a map of `Int32 -> Int32` and update the value of a mid-valued key.
    ///
    @Benchmark
    pub def benchmark28(): Bool =
        Map.exists((_, v) -> v == 24, Map.adjust(_ -> 42, 12, range(0, 24)))

    ///
    /// create a map of `Int32 -> Int32` and update the value of a high-valued key.
    ///
    @Benchmark
    pub def benchmark29(): Bool =
        Map.exists((_, v) -> v == 42, Map.adjust(_ -> 42, 23, range(0, 24)))

    ///
    /// create two maps of `Int32 -> Int32` and compute the size of their union.
    ///
    @Benchmark
    pub def benchmark30(): Bool =
        Map.size(Map.union(range(0, 12), range(12, 24))) == 24

    ///
    /// Returns a map containing `x -> x` for all `x` between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Map.empty()` if `b >= e`.
    ///
    def range(b: Int32, e: Int32): Map[Int32, Int32] =
        if (b >= e) Map.empty() else rangeHelper(b, e - 1, Map.empty())

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, n: Int32, acc: Map[Int32, Int32]): Map[Int32, Int32] =
        if (n < b) acc else rangeHelper(b, n - 1, Map.insert(n, n, acc))

}

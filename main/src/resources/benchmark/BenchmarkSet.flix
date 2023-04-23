mod BenchmarkSet {

    ///
    /// create a set of `Int32` and compute the size of the result.
    ///
    @benchmark
    pub def benchmark01(): Bool =
        Set.size(Set.range(0, 24)) == 24

    ///
    /// create a set of `Int32` and check if it contains a low-valued element.
    ///
    @benchmark
    pub def benchmark02(): Bool =
        Set.memberOf(0, Set.range(0, 24))

    ///
    /// create a set of `Int32` and check if it contains a mid-valued element.
    ///
    @benchmark
    pub def benchmark03(): Bool =
        Set.memberOf(12, Set.range(0, 24))

    ///
    /// create a set of `Int32` and check if it contains a high-valued element.
    ///
    @benchmark
    pub def benchmark04(): Bool =
        Set.memberOf(23, Set.range(0, 24))

    ///
    /// create a set of `Int32` and check if it contains a non-existent element.
    ///
    @benchmark
    pub def benchmark05(): Bool =
        not Set.memberOf(24, Set.range(0, 24))

    ///
    /// create a set of `Int32`, delete the lower half of the elements and compute the size.
    ///
    @benchmark
    pub def benchmark06(): Bool =
        Set.size(Set.foldLeft((acc, y) -> Set.remove(y, acc), Set.range(0, 24), Set.range(0, 12))) == 12

    ///
    /// create a set of `Int32`, delete the upper half of the elements and compute the size.
    ///
    @benchmark
    pub def benchmark07(): Bool =
        Set.size(Set.foldLeft((acc, y) -> Set.remove(y, acc), Set.range(0, 24), Set.range(12, 24))) == 12

    ///
    /// create two sets of `Int32`, find their difference and compute the size.
    ///
    @benchmark
    pub def benchmark08(): Bool =
        Set.size(Set.difference(Set.range(0, 12), Set.range(12, 24))) == 12

    ///
    /// create a set of `Int32` and check if at least one element satisfies a predicate.
    ///
    @benchmark
    pub def benchmark09(): Bool =
        Set.exists(x -> x > 0, Set.range(0, 24))

    ///
    /// create a set of `Int32` and check if at least one element satisfies a predicate.
    ///
    @benchmark
    pub def benchmark10(): Bool =
        Set.exists(x -> x > 22, Set.range(0, 24))

    ///
    /// create a set of `Int32` and check if at least one element satisfies a predicate.
    ///
    @benchmark
    pub def benchmark11(): Bool =
        not Set.exists(x -> x > 23, Set.range(0, 24))

    ///
    /// create a set of `Int32`, filter out the even values and compute the size.
    ///
    @benchmark
    pub def benchmark12(): Bool =
        Set.size(Set.filter(x -> Int32.rem(x, 2) == 0, Set.range(0, 24))) == 12

    ///
    /// create a set of `Int32` and find the first element that satisfies a predicate, searching left to right.
    ///
    @benchmark
    pub def benchmark13(): Bool =
        Set.findLeft(x -> x == 23, Set.range(0, 24)) == Some(23)

    ///
    /// create a set of `Int32` and find the first element that satisfies a predicate, searching right to left.
    ///
    @benchmark
    pub def benchmark14(): Bool =
        Set.findRight(x -> x == 0, Set.range(0, 24)) == Some(0)

    ///
    /// create a set of `Int32` and compute its sum via foldLeft.
    ///
    @benchmark
    pub def benchmark15(): Bool =
        Set.foldLeft((x, y) -> x + y, 0, Set.range(0, 24)) == 276

    ///
    /// create a set of `Int32` and compute its sum via foldRight.
    ///
    @benchmark
    pub def benchmark16(): Bool =
        Set.foldRight((x, y) -> x + y, 0, Set.range(0, 24)) == 276

    ///
    /// create a set of `Int32` and check if all elements satisfy a predicate.
    ///
    @benchmark
    pub def benchmark17(): Bool =
        Set.forAll(x -> x < 24, Set.range(0, 24))

    ///
    /// create two sets of `Int32`, find their intersection and compute the size.
    ///
    @benchmark
    pub def benchmark18(): Bool =
        Set.size(Set.intersection(Set.range(0, 12), Set.range(0, 24))) == 12

    ///
    /// create two sets of `Int32` and check if the first is a subset of the second.
    ///
    @benchmark
    pub def benchmark19(): Bool =
        Set.isSubsetOf(Set.range(0, 24), Set.range(0, 24))

    ///
    /// create two sets of `Int32` and check if the first is a proper subset of the second.
    ///
    @benchmark
    pub def benchmark20(): Bool =
        Set.isSubsetOf(Set.range(0, 12), Set.range(0, 24))

    ///
    /// create a set of `Int32`, increment each integer by one and compute the size.
    ///
    @benchmark
    pub def benchmark21(): Bool =
        Set.size(Set.map(x -> x + 1, Set.range(0, 24))) == 24

    ///
    /// create a set of `Int32`, partition it into even and odd numbers and compute the size of the first partition.
    ///
    @benchmark
    pub def benchmark22(): Bool =
        Set.size(fst(Set.partition(x -> Int32.rem(x, 2) == 0, Set.range(0, 24)))) == 12

    ///
    /// create a set of `Int32` and compute its sum via reduceLeft.
    ///
    @benchmark
    pub def benchmark23(): Bool =
        Set.reduceLeft((x, y) -> x + y, Set.range(0, 24)) == Some(276)

    ///
    /// create a set of `Int32` and compute its sum via reduceRight.
    ///
    @benchmark
    pub def benchmark24(): Bool =
        Set.reduceRight((x, y) -> x + y, Set.range(0, 24)) == Some(276)

    ///
    /// create a set of `Int32` and replace a low-valued element with a high-valued element.
    ///
    @benchmark
    pub def benchmark25(): Bool =
        Set.memberOf(24, Set.replace(from = 0, to = 24, Set.range(0, 24)))

    ///
    /// create a set of `Int32` and replace a mid-valued element with a high-valued element.
    ///
    @benchmark
    pub def benchmark26(): Bool =
        Set.memberOf(24, Set.replace(from = 12, to = 24, Set.range(0, 24)))

    ///`
    /// create a set of `Int32` and replace a high-valued element with a low valued-element.
    ///
    @benchmark
    pub def benchmark27(): Bool =
        Set.memberOf(-1, Set.replace(from = 23, to = -1, Set.range(0, 24)))

    ///
    /// create a set of 4 `Int32` and compute the size of its subsets.
    ///
    @benchmark
    pub def benchmark28(): Bool =
        Set.size(Set.subsets(Set.range(0, 3))) == 8

    ///
    /// create two sets of `Int32` and compute the size of their union.
    ///
    @benchmark
    pub def benchmark29(): Bool =
        Set.size(Set.union(Set.range(0, 12), Set.range(12, 24))) == 24

    ///
    /// create a set of `Int32`, compute all its subsets, flatten (union) them and compute the size of the resulting set.
    ///
    @benchmark
    pub def benchmark30(): Bool =
        Set.size(Set.flatten(Set.subsets(Set.range(0, 3)))) == 3

}

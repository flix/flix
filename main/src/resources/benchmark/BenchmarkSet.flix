namespace BenchmarkSet {

    ///
    /// create a set of integers and compute the size of the result.
    ///
    @benchmark
    pub def benchmark01(): Bool = Set.size(List.toSet(List.range(0, 75))) == 75

    ///
    /// create a set of integers and count the numbers divisible by 5.
    ///
    @benchmark
    pub def benchmark02(): Bool = Set.count(x -> x % 5 == 0, List.toSet(List.range(0, 75))) == 15

    ///
    /// create a set of integers, delete half of the elements, and compute the size.
    ///
    @benchmark
    pub def benchmark03(): Bool = Set.size(Set.fold((x, y) -> Set.delete(y, x), List.toSet(List.range(0, 30)), List.toSet(List.range(0, 15)))) == 15

    ///
    /// create two sets of integers, find their difference, and compute the size.
    ///
    @benchmark
    pub def benchmark04(): Bool = Set.size(Set.difference(List.toSet(List.range(0, 25)), List.toSet(List.range(25, 50)))) == 25

    ///
    /// create a set of integers and check if at least one element satisfies a predicate.
    ///
    @benchmark
    pub def benchmark05(): Bool = Set.exists(x -> x > 76, List.toSet(List.range(0, 75)))

    ///
    /// create a set of integers, filter out half of the elements, and compute the size.
    ///
    @benchmark
    pub def benchmark06(): Bool = Set.size(Set.filter(x -> x >= 25, List.toSet(List.range(0, 50)))) == 25

    ///
    /// create a set of integers and find the first element that satifies a predicate.
    ///
    @benchmark
    pub def benchmark07(): Bool = Set.find(x -> x > 73, List.toSet(List.range(0, 75))) == Some(74)

    ///
    /// create a set of integers and compute its sum via foldLeft.
    ///
    @benchmark
    pub def benchmark08(): Bool = Set.foldLeft((x, y) -> x + y, 0, List.toSet(List.range(0, 75))) == 2775

    ///
    /// create a set of integers and compute its sum via foldRight.
    ///
    @benchmark
    pub def benchmark09(): Bool = Set.foldRight((x, y) -> x + y, 0, List.toSet(List.range(0, 75))) == 2775

    ///
    /// create a set of integers and check if all elements satisfy a predicate.
    ///
    @benchmark
    pub def benchmark10(): Bool = Set.forall(x -> x < 74, List.toSet(List.range(0, 75)))

    ///
    /// create two sets of integers, find their intersection, and compute the size.
    ///
    @benchmark
    pub def benchmark11(): Bool = Set.size(Set.intersection(List.toSet(List.range(0, 30)), List.toSet(List.range(15, 45)))) == 15

    ///
    /// create two sets of integers and check if the first is a subset of the second.
    ///
    @benchmark
    pub def benchmark12(): Bool = Set.isSubsetOf(List.toSet(List.range(0, 25)), List.toSet(List.range(0, 50)))

    ///
    /// create a set of integers, increment each integer by one, and compute the size.
    ///
    @benchmark
    pub def benchmark13(): Bool = Set.size(Set.map(x -> x + 1, List.toSet(List.range(0, 50)))) == 50

    ///
    /// create a set of integers, partition it, and compute the size of the first partition.
    ///
    @benchmark
    pub def benchmark14(): Bool =
        let partition = Set.partition(x -> x >= 25, List.toSet(List.range(0, 50)));
        let (fst, _) = partition;
            Set.size(fst) == 25

    ///
    /// create a set of integers and compute its sum via reduceLeft.
    ///
    @benchmark
    pub def benchmark15(): Bool = Set.reduceLeft((x, y) -> x + y, List.toSet(List.range(0, 50))) == Some(1225)

    ///
    /// create a set of integers and compute its sum via reduceRight.
    ///
    @benchmark
    pub def benchmark16(): Bool = Set.reduceRight((x, y) -> x + y, List.toSet(List.range(0, 50))) == Some(1225)

    ///
    /// create a set of integers and replace an element.
    ///
    @benchmark
    pub def benchmark17(): Bool = Set.memberOf(50, Set.replace(49, 50, List.toSet(List.range(0, 50))))

    ///
    /// create a set of integers and compute the size of all its subsets.
    ///
    @benchmark
    pub def benchmark18(): Bool = Set.size(Set.subsets(List.toSet(List.range(0, 7)))) == 128

    ///
    /// create two sets of integers and compute the size of their union.
    ///
    @benchmark
    pub def benchmark19(): Bool = Set.size(Set.union(List.toSet(List.range(0, 30)), List.toSet(List.range(20, 40)))) == 40

    ///
    /// create a set of integers, compute all its subsets, flatten (union) them, and compute the size of the resulting set.
    ///
    @benchmark
    pub def benchmark20(): Bool = Set.size(Set.flatten(Set.subsets(List.toSet(List.range(0, 6))))) == 6

}
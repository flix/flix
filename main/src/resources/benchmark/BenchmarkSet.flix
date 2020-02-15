namespace BenchmarkSet {

    ///
    /// create a set of integers and compute the size of the result.
    ///
    @benchmark
    pub def benchmark01(): Bool = Set.size(List.toSet(List.range(0, 75))) `assertEq!` 75

    ///
    /// create a set of integers and count the numbers divisible by 5.
    ///
    @benchmark
    pub def benchmark02(): Bool = Set.count(x -> x % 5 == 0, List.toSet(List.range(0, 75))) `assertEq!` 15

    ///
    /// create a set of integers, delete half and compute the size.
    ///
    @benchmark
    pub def benchmark03(): Bool = Set.size(Set.fold((x, y) -> Set.delete(y, x), List.toSet(List.range(0, 50)), List.toSet(List.range(0, 25)))) `assertEq!` 25

}
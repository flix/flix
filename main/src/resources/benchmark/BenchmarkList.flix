mod BenchmarkList {

    ///
    /// create a list of integers and compute the length of the result.
    ///
    @benchmark
    pub def benchmark01(): Bool = List.length(List.range(0, 1000)) == 1000

    ///
    /// create a list of integers and return the first half the elements.
    ///
    @benchmark
    pub def benchmark02(): Bool = List.length(List.take(500, List.range(0, 1000))) == 500

    ///
    /// create a list of integers and return the last half the elements.
    ///
    @benchmark
    pub def benchmark03(): Bool = List.length(List.drop(500, List.range(0, 1000))) == 500

    ///
    /// create a list of integers, reverse it, and compute the length of the result.
    ///
    @benchmark
    pub def benchmark04(): Bool = List.length(List.reverse(List.range(0, 1000))) == 1000

    ///
    /// create a list of integers, filter all its even numbers, and compute the length of the result.
    ///
    @benchmark
    pub def benchmark05(): Bool = List.length(List.filter(x -> Int32.rem(x, 2) == 0, List.range(0, 1000))) == 500

    ///
    /// create a list of integers, append it to a list of integers, and compute the length of the result.
    ///
    @benchmark
    pub def benchmark06(): Bool = List.length(List.range(0, 1000) ::: List.range(0, 1000)) == 2000

    ///
    /// create a list of integers, increment each integer by one, and compute the length of the result.
    ///
    @benchmark
    pub def benchmark07(): Bool = List.length(List.map(x -> x + 1, List.range(0, 1000))) == 1000

    ///
    /// create a list of integers, flatMap it over lists of integers, and compute the length of the result.
    ///
    @benchmark
    pub def benchmark08(): Bool = List.length(List.flatMap(x -> List.repeat(20, x), List.range(0, 20))) == 400

    ///
    /// create a list of integers and compute its sum via foldLeft.
    ///
    @benchmark
    pub def benchmark09(): Bool = List.foldLeft((x, y) -> x + y, 0, List.range(0, 1000)) == 499500

    ///
    /// create a list of integers and compute its sum via foldRight.
    ///
    @benchmark
    pub def benchmark10(): Bool = List.foldRight((x, acc) -> x + acc, 0, List.range(0, 1000)) == 499500

    ///
    /// create two lists of integers, zip them, and compute the length of the result.
    ///
    @benchmark
    pub def benchmark11(): Bool = List.length(List.zip(List.range(0, 1000), List.range(0, 1000))) == 1000

    ///
    /// create a list of pairs, unzip it, and compute the length of the result.
    ///
    @benchmark
    pub def benchmark12(): Bool =
        let (xs, ys) = List.unzip(List.map(x -> (x, 2 * x), List.range(0, 1000)));
            (List.length(xs) + List.length(ys)) == 2000

    ///
    /// create two lists of integers, check that each integer of the first list exists in the latter list.
    ///
    @benchmark
    pub def benchmark13(): Bool =
        let xs = List.range(0, 100);
        let ys = List.range(0, 100);
        let zs = List.map(y -> List.memberOf(y, xs), ys);
            List.length(zs) == 100

    ///
    /// create two lists of integers, check if the cube of each integer in the first list exists in the latter list.
    ///
    @benchmark
    pub def benchmark14(): Bool =
        let xs = List.range(0, 50);
        let ys = List.range(0, 50);
        let zs = List.map(y -> List.exists(x -> x * x == y, xs), ys);
            List.length(zs) == 50

    ///
    /// intersperses an integer into a list of integers.
    ///
    @benchmark
    pub def benchmark15(): Bool = List.length(List.intersperse(42, List.range(0, 1000))) == 1999

    ///
    /// create two lists of integers, zip them, unzip them, and compute the length of the result.
    ///
    @benchmark
    pub def benchmark16(): Bool =
        let (xs, ys) = List.unzip(List.zip(List.range(0, 1000), List.range(0, 1000)));
            (List.length(xs) + List.length(ys)) == 2000

    ///
    /// creates a list and searches for an item from the left.
    ///
    @benchmark
    pub def benchmark17(): Bool = List.findLeft(x -> x == 500, List.range(0, 1000)) == Some(500)

    ///
    /// creates a list and searches for an item from the right.
    ///
    @benchmark
    pub def benchmark18(): Bool = List.findRight(x -> x == 500, List.range(0, 1000)) == Some(500)

}

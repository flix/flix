namespace BenchmarkList {

    ///
    /// create a list of integers and compute the length of the result.
    ///
    @benchmark
    def benchmark01(): Int = List.length(List.range(0, 100 * 1000))

    ///
    /// create a list of integers and return the first half the elements.
    ///
    @benchmark
    def benchmark02(): Int = List.length(List.take(50 * 1000, List.range(0, 100 * 1000)))

    ///
    /// create a list of integers and return the last half the elements.
    ///
    @benchmark
    def benchmark03(): Int = List.length(List.drop(50 * 1000, List.range(0, 100 * 1000)))

    ///
    /// create a list of integers, reverse it, and compute the length of the result.
    ///
    @benchmark
    def benchmark04(): Int = List.length(List.reverse(List.range(0, 100 * 1000)))

    ///
    /// create a list of integers, filter all its even numbers, and compute the length of the result.
    ///
    @benchmark
    def benchmark05(): Int = List.length(List.filter(x -> x % 2 == 0, List.range(0, 100 * 1000)))

    ///
    /// create a list of integers, append it to a list of 1 million integers, and compute the length of the result.
    ///
    @benchmark
    def benchmark06(): Int = List.length(List.range(0, 100 * 1000) ::: List.range(0, 100 * 1000))

    ///
    /// create a list of integers, increment each integer by one, and compute the length of the result.
    ///
    @benchmark
    def benchmark07(): Int = List.length(List.map(x -> x + 1, List.range(0, 100 * 1000)))

    ///
    /// create a list of integers, flatMap it over lists of integers, and compute the length of the result.
    ///
    @benchmark
    def benchmark08(): Int = List.length(List.flatMap(x -> List.repeat(x, 100), List.range(0, 1000)))

    ///
    /// create a list of integers and compute its sum via foldLeft.
    ///
    @benchmark
    def benchmark09(): Int = List.foldLeft((x, y) -> x + y, 0, List.range(0, 100 * 1000))

    ///
    /// create a list of integers and compute its sum via foldRight.
    ///
    @benchmark
    def benchmark10(): Int = List.foldRight((x, y) -> x + y, 0, List.range(0, 100 * 1000))

    ///
    /// create two lists of integers, zip them, and compute the length of the result.
    ///
    @benchmark
    def benchmark11(): Int = List.length(List.zip(List.range(0, 100 * 1000), List.range(0, 100 * 1000)))

    ///
    /// create a list of pairs, unzip it, and compute the length of the result.
    ///
    @benchmark
    def benchmark12(): Int =
        let (xs, ys) = List.unzip(List.map(x -> (x, 2 * x), List.range(0, 100 * 1000)));
            List.length(xs) + List.length(ys)

    ///
    /// create two lists of integers, check that each integer of the first list exists in the latter list.
    ///
    @benchmark
    def benchmark13(): Int =
        let xs = List.range(0, 1000);
        let ys = List.range(0, 1000);
        let zs = List.map(y -> List.memberOf(y, xs), ys);
            List.length(zs)

    ///
    /// create two lists of integers, check if the cube of each integer in the first list exists in the latter list.
    ///
    @benchmark
    def benchmark14(): Int =
        let xs = List.range(0, 1000);
        let ys = List.range(0, 1000);
        let zs = List.map(y -> List.exists(x -> x * x == y, xs), ys);
            List.length(zs)

    ///
    /// intersperses an integer into a list of integers.
    ///
    @benchmark
    def benchmark15(): Int = List.length(List.intersperse(42, List.range(0, 100 * 1000)))

    ///
    /// create two lists of integers, zip them, unzip them, and compute the length of the result.
    ///
    @benchmark
    def benchmark16(): Int =
        let (xs, ys) = List.unzip(List.zip(List.range(0, 100 * 1000), List.range(0, 100 * 1000)));
            List.length(xs) + List.length(ys)

    ///
    /// creates a list and searches for an item from the left.
    ///
    @benchmark
    def benchmark17(): Option[Int] = List.findLeft(x -> x == 50 * 1000, List.range(0, 100 * 1000))

    ///
    /// creates a list and searches for an item from the right.
    ///
    @benchmark
    def benchmark18(): Option[Int] = List.findRight(x -> x == 50 * 1000, List.range(0, 100 * 1000))

    // A new type wrapper.
    type Celsius = Celsius(Int)

    ///
    /// creates a list of integers (celsius) and sums the result.
    ///
    @benchmark
    def benchmark19(): Int =
        let f = (x, y) -> match y with {
            case Celsius(z) => x + z
        };
        List.foldLeft(f, 0, List.map(Celsius, List.range(0, 100 * 1000)))

}

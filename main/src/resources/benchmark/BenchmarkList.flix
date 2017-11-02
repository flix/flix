namespace BenchmarkList {

    ///
    /// create a list of integers and compute the length of the result.
    ///
    @benchmark
    def benchmark01(): Bool = List.length(List.range(0, 100 * 1000)) `assertEq!` 100000

    ///
    /// create a list of integers and return the first half the elements.
    ///
    @benchmark
    def benchmark02(): Bool = List.length(List.take(50 * 1000, List.range(0, 100 * 1000))) `assertEq!` 50000

    ///
    /// create a list of integers and return the last half the elements.
    ///
    @benchmark
    def benchmark03(): Bool = List.length(List.drop(50 * 1000, List.range(0, 100 * 1000))) `assertEq!` 50000

    ///
    /// create a list of integers, reverse it, and compute the length of the result.
    ///
    @benchmark
    def benchmark04(): Bool = List.length(List.reverse(List.range(0, 100 * 1000))) `assertEq!` 100000

    ///
    /// create a list of integers, filter all its even numbers, and compute the length of the result.
    ///
    @benchmark
    def benchmark05(): Bool = List.length(List.filter(x -> x % 2 == 0, List.range(0, 100 * 1000))) `assertEq!` 50000

    ///
    /// create a list of integers, append it to a list of integers, and compute the length of the result.
    ///
    @benchmark
    def benchmark06(): Bool = List.length(List.range(0, 100 * 1000) ::: List.range(0, 100 * 1000)) `assertEq!` 200000

    ///
    /// create a list of integers, increment each integer by one, and compute the length of the result.
    ///
    @benchmark
    def benchmark07(): Bool = List.length(List.map(x -> x + 1, List.range(0, 100 * 1000))) `assertEq!` 100000

    ///
    /// create a list of integers, flatMap it over lists of integers, and compute the length of the result.
    ///
    @benchmark
    def benchmark08(): Bool = List.length(List.flatMap(x -> List.repeat(x, 100), List.range(0, 1000))) `assertEq!` 100000

    ///
    /// create a list of integers and compute its sum via foldLeft.
    ///
    @benchmark
    def benchmark09(): Bool = List.foldLeft((x, y) -> x + y, 0, List.range(0, 100 * 1000)) `assertEq!` 704982704

    ///
    /// create a list of integers and compute its sum via foldRight.
    ///
    @benchmark
    def benchmark10(): Bool = List.foldRight((x, y) -> x + y, 0, List.range(0, 100 * 1000)) `assertEq!` 704982704

    ///
    /// create two lists of integers, zip them, and compute the length of the result.
    ///
    @benchmark
    def benchmark11(): Bool = List.length(List.zip(List.range(0, 100 * 1000), List.range(0, 100 * 1000))) `assertEq!` 100000

    ///
    /// create a list of pairs, unzip it, and compute the length of the result.
    ///
    @benchmark
    def benchmark12(): Bool =
        let (xs, ys) = List.unzip(List.map(x -> (x, 2 * x), List.range(0, 100 * 1000)));
            (List.length(xs) + List.length(ys)) `assertEq!` 200000

    ///
    /// create two lists of integers, check that each integer of the first list exists in the latter list.
    ///
    @benchmark
    def benchmark13(): Bool =
        let xs = List.range(0, 1000);
        let ys = List.range(0, 1000);
        let zs = List.map(y -> List.memberOf(y, xs), ys);
            List.length(zs) `assertEq!` 1000

    ///
    /// create two lists of integers, check if the cube of each integer in the first list exists in the latter list.
    ///
    @benchmark
    def benchmark14(): Bool =
        let xs = List.range(0, 1000);
        let ys = List.range(0, 1000);
        let zs = List.map(y -> List.exists(x -> x * x == y, xs), ys);
            List.length(zs) `assertEq!` 1000

    ///
    /// intersperses an integer into a list of integers.
    ///
    @benchmark
    def benchmark15(): Bool = List.length(List.intersperse(42, List.range(0, 100 * 1000))) `assertEq!` 199999

    ///
    /// create two lists of integers, zip them, unzip them, and compute the length of the result.
    ///
    @benchmark
    def benchmark16(): Bool =
        let (xs, ys) = List.unzip(List.zip(List.range(0, 100 * 1000), List.range(0, 100 * 1000)));
            (List.length(xs) + List.length(ys)) `assertEq!` 200000

    ///
    /// creates a list and searches for an item from the left.
    ///
    @benchmark
    def benchmark17(): Bool = List.findLeft(x -> x == 50 * 1000, List.range(0, 100 * 1000)) `assertEq!` Some(50000)

    ///
    /// creates a list and searches for an item from the right.
    ///
    @benchmark
    def benchmark18(): Bool = List.findRight(x -> x == 50 * 1000, List.range(0, 100 * 1000)) `assertEq!` Some(50000)

    ///
    /// creates a list of integers (in celsius) and sums them (as integers).
    ///
    @benchmark
    def benchmark19(): Bool =
        let f = (x, y) -> match y with {
            case Celsius(z) => x + z
        };
        List.foldLeft(f, 0, List.map(Celsius, List.range(0, 100 * 1000))) `assertEq!` 704982704

    ///
    /// creates a list of integers (in celsius) and sums them (as celsius).
    ///
    @benchmark
    def benchmark20(): Bool =
        let f = (x, y) ->
            let Celsius(a) = x;
            let Celsius(b) = y;
                Celsius(a + b);
        List.foldLeft(f, Celsius(0), List.map(Celsius, List.range(0, 100 * 1000)))  `assertEq!` Celsius(704982704)

    // A type safe wrapper of integers.
    type Celsius = Celsius(Int)

}

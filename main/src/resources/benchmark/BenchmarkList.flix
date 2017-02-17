namespace BenchmarkList {

    // create a list of 1 million integers and compute the length of the result.
    @benchmark
    def benchmark01: Int = List.length(List.range(0, 1000 * 1000))

    // create a list of 1 million integers, reverse it, and compute the length of the result.
    @benchmark
    def benchmark02: Int = List.length(List.reverse(List.range(0, 1000 * 1000)))

    // create a list of 1 million integers, filter all its even numbers, and compute the length of the result.
    @benchmark
    def benchmark03: Int = List.length(List.filter(x -> x % 2 == 0, List.range(0, 1000 * 1000)))

    // create a list of 1 million integers, append it to a list of 1 million integers, and compute the length of the result.
    @benchmark
    def benchmark04: Int = List.length(List.range(0, 1000 * 1000) ::: List.range(0, 1000 * 1000))

    // create a list of 1 million integers, increment each integer by one, and compute the length of the result.
    @benchmark
    def benchmark05: Int = List.length(List.map(x -> x + 1, List.range(0, 1000 * 1000)))

    // create a list of 1 thousand integers, flatMap it over lists of 1 thousand integers, and compute the length of the result.
    @benchmark
    def benchmark06: Int = List.length(List.flatMap(x -> List.repeat(x, 1000), List.range(0, 1000)))

    // create a list of 1 million integers and compute its sum via foldLeft.
    @benchmark
    def benchmark07: Int = List.foldLeft((x, y) -> x + y, 0, List.range(0, 1000 * 1000))

    // create a list of 1 million integers and compute its sum via foldRight.
    @benchmark
    def benchmark08: Int = List.foldRight((x, y) -> x + y, 0, List.range(0, 1000 * 1000))

    // create two lists of 1 million integers, zip them, and compute the length of the result.
    @benchmark
    def benchmark09: Int = List.length(List.zip(List.range(0, 1000 * 1000), List.range(0, 1000 * 1000)))

}

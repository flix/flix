namespace BenchmarkList {

    def benchmark01: Int =
        List.foldLeft((x, y) -> x + y, 0,
            List.map(x -> x + 1,
                List.range(1, 1000 * 1000)))

}

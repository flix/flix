namespace Channel {

    ///
    /// Parallel implementation of `List.map`.
    ///
    pub def parMap[a, b](f: a -> b, xs: List[a]): List[b] =
        let c = chan b;
        List.map(x -> spawn c <- f(x), xs);
        List.map(x -> (<- c), xs)

    ///
    /// Parallel implementation of `List.flatMap`.
    ///
    pub def parFlatMap[a, b](f: a -> List[b], xs: List[a]): List[b] =
        let c = chan List[b];
        List.map(x -> spawn c <- f(x), xs);
        List.flatMap(x -> (<- c), xs)


    // TODO: Move tests.

    @test
    def testParMap01(): Bool =
        let sum = xs -> List.foldLeft((x, y) -> x + y, 0, xs);
        ((Nil) |> parMap(x -> x * 2) |> sum) == 12

    @test
    def testParMap02(): Bool =
        let sum = xs -> List.foldLeft((x, y) -> x + y, 0, xs);
        ((1 :: Nil) |> parMap(x -> x * 2) |> sum) == 2

    @test
    def testParMap03(): Bool =
        let sum = xs -> List.foldLeft((x, y) -> x + y, 0, xs);
        ((1 :: 2 :: Nil) |> parMap(x -> x * 2) |> sum) == 6

    @test
    def testParMap04(): Bool =
        let sum = xs -> List.foldLeft((x, y) -> x + y, 0, xs);
        ((1 :: 2 :: 3 :: Nil) |> parMap(x -> x * 2) |> sum) == 12

}

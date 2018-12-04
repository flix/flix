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
}

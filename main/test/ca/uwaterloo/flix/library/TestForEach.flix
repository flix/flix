mod TestForEach {
    use ForEach.forEach
    use ForEach.withIndex

    /////////////////////////////////////////////////////////////////////////////
    // withIndex                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def withIndex01(): Bool = region rc {
        let results = MutList.empty(rc);
        Nil |> withIndex |> forEach(pair -> {
            let (i, x) = pair;
            results |> MutList.push((i, x))
        });
        results |> MutList.isEmpty
    }

    @test
    pub def withIndex02(): Bool = region rc {
        let results = MutList.empty(rc);
        (2 :: 4 :: 6 :: 8 :: Nil) |> withIndex |> forEach(pair -> {
            let (i, x) = pair;
            results |> MutList.push((i, x))
        });
        results |> MutList.toList == (0, 2) :: (1, 4) :: (2, 6) :: (3, 8) :: Nil
    }

}

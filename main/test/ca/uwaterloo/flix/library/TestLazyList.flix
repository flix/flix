namespace TestLazyList {

    /////////////////////////////////////////////////////////////////////////////
    // empty / new                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool =
        LazyList.empty(): LazyList[Int32] |> LazyList.toList == Nil

    @test
    def new01(): Bool =
        LazyList.new(): LazyList[Int32] |> LazyList.toList == Nil

}
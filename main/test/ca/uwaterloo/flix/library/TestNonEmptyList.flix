namespace NonEmptyList {

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = head(Nel(123, Nil)) == 123

    @test
    def head02(): Bool = head(Nel(123, 1 :: Nil)) == 123

    @test
    def head03(): Bool = head(Nel(123, 1 :: 2 :: Nil)) == 123


}


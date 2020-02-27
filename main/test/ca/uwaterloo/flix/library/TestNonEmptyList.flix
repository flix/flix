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

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = last(Nel(123, Nil)) == 123

    @test
    def last02(): Bool = last(Nel(123, 1 :: Nil)) == 1

    @test
    def last03(): Bool = last(Nel(123, 1 :: 2 :: Nil)) == 2

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = init(Nel(1, Nil)) == Nil

    @test
    def init02(): Bool = init(Nel(1, 2 :: Nil)) == 1 :: Nil

    @test
    def init03(): Bool = init(Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool = tail(Nel(1, Nil)) == Nil

    @test
    def tail02(): Bool = tail(Nel(1, 2 :: Nil)) == 2 :: Nil

    @test
    def tail03(): Bool = tail(Nel(1, 2 :: 3 :: Nil)) == 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = length(Nel(1, Nil)) == 1

    @test
    def length02(): Bool = length(Nel(1, 2 :: Nil)) == 2

    @test
    def length03(): Bool = length(Nel(1, 2 :: 3 :: Nil)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = Nel(1, Nil) `append` Nel(2, Nil) == Nel(1, 2 :: Nil)

    @test
    def append02(): Bool = Nel(1, 2 :: Nil) `append` Nel(3, Nil) == Nel(1, 2 :: 3 :: Nil)

    @test
    def append03(): Bool = Nel(1, Nil) `append` Nel(2, 3 :: Nil) == Nel(1, 2 :: 3 :: Nil)

    @test
    def append04(): Bool = Nel(1, 2 :: Nil) `append` Nel(3, 4 :: Nil) == Nel(1, 2 :: 3 :: 4 :: Nil)

    @test
    def append05(): Bool = Nel(1, 2 :: 3 :: Nil) `append` Nel(4, 5 :: 6 :: Nil) == Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: Nil)

}


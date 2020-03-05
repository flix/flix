namespace Nel {

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

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = memberOf(0, Nel(1, Nil)) == false

    @test
    def memberOf02(): Bool = memberOf(0, Nel(1, 2 :: Nil)) == false

    @test
    def memberOf03(): Bool = memberOf(0, Nel(1, 2 :: 3 :: Nil)) == false

    @test
    def memberOf04(): Bool = memberOf(1, Nel(1, 2 :: 3 :: Nil))

    @test
    def memberOf05(): Bool = memberOf(2, Nel(1, 2 :: 3 :: Nil))

    @test
    def memberOf06(): Bool = memberOf(3, Nel(1, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = findLeft(x -> x == 0, Nel(1, Nil)) == None

    @test
    def findLeft02(): Bool = findLeft(x -> x == 0, Nel(1, 2 :: Nil)) == None

    @test
    def findLeft03(): Bool = findLeft(x -> x == 0, Nel(1, 2 :: 3 :: Nil)) == None

    @test
    def findLeft04(): Bool = findLeft(x -> x == 1, Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @test
    def findLeft05(): Bool = findLeft(x -> x == 2, Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @test
    def findLeft06(): Bool = findLeft(x -> x == 3, Nel(1, 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = findRight(x -> x == 0, Nel(1, Nil)) == None

    @test
    def findRight02(): Bool = findRight(x -> x == 0, Nel(1, 2 :: Nil)) == None

    @test
    def findRight03(): Bool = findRight(x -> x == 0, Nel(1, 2 :: 3 :: Nil)) == None

    @test
    def findRight04(): Bool = findRight(x -> x == 1, Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @test
    def findRight05(): Bool = findRight(x -> x == 2, Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @test
    def findRight06(): Bool = findRight(x -> x == 3, Nel(1, 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = map(x -> x + 1, Nel(1, Nil)) == Nel(2, Nil)

    @test
    def map02(): Bool = map(x -> x + 1, Nel(1, 2 :: Nil)) == Nel(2, 3 :: Nil)

    @test
    def map03(): Bool = map(x -> x + 1, Nel(1, 2 :: 3 :: Nil)) == Nel(2, 3 :: 4 :: Nil)

}


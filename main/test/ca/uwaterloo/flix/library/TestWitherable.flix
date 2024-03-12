mod TestWitherable {

    use Witherable.{wither, sequenceWither};

    /////////////////////////////////////////////////////////////////////////////
    // wither                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def wither01(): Bool  =
        let xs = Nil;
        wither(x -> Some(x + 1) |> Some, xs) == Some(Nil)

    @test
    def wither02(): Bool  =
        let xs = Nil;
        wither(_ -> Some((None: Option[Int32])), xs) == Some(Nil)

    @test
    def wither03(): Bool  =
        let xs = Nil;
        wither(_ -> (None: Option[Option[Int32]]), xs) == Some(Nil)

    @test
    def wither04(): Bool  =
        let xs = 1 :: Nil;
        wither(x -> Some(x + 1) |> Some, xs) == Some(2 :: Nil)

    @test
    def wither05(): Bool  =
        let xs = 1 :: Nil;
        wither(_ -> Some((None: Option[Int32])), xs) == Some(Nil)

    @test
    def wither06(): Bool  =
        let xs = 1 :: 2 :: Nil;
        wither(x -> Some(if (x > 1) None else Some(x - 1)), xs) == Some(0 :: Nil)

    @test
    def wither07(): Bool  =
        let xs = 1 :: 2 :: Nil;
        wither(_ ->( None: Option[Option[Int32]]), xs) == None

    @test
    def wither08(): Bool  =
        let xs = 1 :: 2 :: 3 :: Nil;
        wither(_ -> Some((None: Option[Int32])), xs) == Some(Nil)


    /////////////////////////////////////////////////////////////////////////////
    // sequenceWither                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequenceWither01(): Bool =
        let xs: List[Option[Option[Int32]]] = Nil;
        sequenceWither(xs) == Some(Nil)

    @test
    def sequenceWither02(): Bool =
        let xs: List[Option[Option[Int32]]] = None :: Nil;
        sequenceWither(xs) == None

    @test
    def sequenceWither03(): Bool =
        let xs: List[Option[Option[Int32]]] = Some(None) :: Nil;
        sequenceWither(xs) == Some(Nil)

    @test
    def sequenceWither04(): Bool =
        let xs = Some(Some(1)) :: Nil;
        sequenceWither(xs) == Some(1 :: Nil)

    @test
    def sequenceWither05(): Bool =
        let xs = Some(Some(1)) :: Some(Some(2)) :: Nil;
        sequenceWither(xs) == Some(1 :: 2 :: Nil)

    @test
    def sequenceWither06(): Bool =
        let xs = Some(Some(1)) :: Some(None) :: Nil;
        sequenceWither(xs) == Some(1 :: Nil)

    @test
    def sequenceWither07(): Bool =
        let xs = Some(Some(1)) :: Some(None) :: None :: Nil;
        sequenceWither(xs) == None

}

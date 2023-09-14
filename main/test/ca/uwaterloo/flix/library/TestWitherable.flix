mod TestWitherable {

    use Witherable.{wither, sequenceWither};

    /////////////////////////////////////////////////////////////////////////////
    // wither                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def wither01(): Bool  =
        let xs = Nil;
        wither(x -> Some(x + 1) |> Some, xs) == Some(Nil)

    @Test
    def wither02(): Bool  =
        let xs = Nil;
        wither(_ -> Some(None: Option[Int32]), xs) == Some(Nil)

    @Test
    def wither03(): Bool  =
        let xs = Nil;
        wither(_ -> None: Option[Option[Int32]], xs) == Some(Nil)

    @Test
    def wither04(): Bool  =
        let xs = 1 :: Nil;
        wither(x -> Some(x + 1) |> Some, xs) == Some(2 :: Nil)

    @Test
    def wither05(): Bool  =
        let xs = 1 :: Nil;
        wither(_ -> Some(None: Option[Int32]), xs) == Some(Nil)

    @Test
    def wither06(): Bool  =
        let xs = 1 :: 2 :: Nil;
        wither(x -> Some(if (x > 1) None else Some(x - 1)), xs) == Some(0 :: Nil)

    @Test
    def wither07(): Bool  =
        let xs = 1 :: 2 :: Nil;
        wither(_ -> None: Option[Option[Int32]], xs) == None

    @Test
    def wither08(): Bool  =
        let xs = 1 :: 2 :: 3 :: Nil;
        wither(_ -> Some(None: Option[Int32]), xs) == Some(Nil)


    /////////////////////////////////////////////////////////////////////////////
    // sequenceWither                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequenceWither01(): Bool =
        let xs: List[Option[Option[Int32]]] = Nil;
        sequenceWither(xs) == Some(Nil)

    @Test
    def sequenceWither02(): Bool =
        let xs: List[Option[Option[Int32]]] = None :: Nil;
        sequenceWither(xs) == None

    @Test
    def sequenceWither03(): Bool =
        let xs: List[Option[Option[Int32]]] = Some(None) :: Nil;
        sequenceWither(xs) == Some(Nil)

    @Test
    def sequenceWither04(): Bool =
        let xs = Some(Some(1)) :: Nil;
        sequenceWither(xs) == Some(1 :: Nil)

    @Test
    def sequenceWither05(): Bool =
        let xs = Some(Some(1)) :: Some(Some(2)) :: Nil;
        sequenceWither(xs) == Some(1 :: 2 :: Nil)

    @Test
    def sequenceWither06(): Bool =
        let xs = Some(Some(1)) :: Some(None) :: Nil;
        sequenceWither(xs) == Some(1 :: Nil)

    @Test
    def sequenceWither07(): Bool =
        let xs = Some(Some(1)) :: Some(None) :: None :: Nil;
        sequenceWither(xs) == None

}

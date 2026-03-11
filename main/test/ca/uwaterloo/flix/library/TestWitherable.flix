mod TestWitherable {

    use Assert.{assertEq};
    use Witherable.{wither, sequenceWither};

    /////////////////////////////////////////////////////////////////////////////
    // wither                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def wither01(): Unit \ Assert =
        let xs = Nil;
        assertEq(expected = Some(Nil), wither(x -> Some(x + 1) |> Some, xs))

    @Test
    def wither02(): Unit \ Assert =
        let xs = Nil;
        assertEq(expected = Some(Nil), wither(_ -> Some((None: Option[Int32])), xs))

    @Test
    def wither03(): Unit \ Assert =
        let xs = Nil;
        assertEq(expected = Some(Nil), wither(_ -> (None: Option[Option[Int32]]), xs))

    @Test
    def wither04(): Unit \ Assert =
        let xs = 1 :: Nil;
        assertEq(expected = Some(2 :: Nil), wither(x -> Some(x + 1) |> Some, xs))

    @Test
    def wither05(): Unit \ Assert =
        let xs = 1 :: Nil;
        assertEq(expected = Some(Nil), wither(_ -> Some((None: Option[Int32])), xs))

    @Test
    def wither06(): Unit \ Assert =
        let xs = 1 :: 2 :: Nil;
        assertEq(expected = Some(0 :: Nil), wither(x -> Some(if (x > 1) None else Some(x - 1)), xs))

    @Test
    def wither07(): Unit \ Assert =
        let xs = 1 :: 2 :: Nil;
        assertEq(expected = None, wither(_ ->( None: Option[Option[Int32]]), xs))

    @Test
    def wither08(): Unit \ Assert =
        let xs = 1 :: 2 :: 3 :: Nil;
        assertEq(expected = Some(Nil), wither(_ -> Some((None: Option[Int32])), xs))


    /////////////////////////////////////////////////////////////////////////////
    // sequenceWither                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequenceWither01(): Unit \ Assert =
        let xs: List[Option[Option[Int32]]] = Nil;
        assertEq(expected = Some(Nil), sequenceWither(xs))

    @Test
    def sequenceWither02(): Unit \ Assert =
        let xs: List[Option[Option[Int32]]] = None :: Nil;
        assertEq(expected = None, sequenceWither(xs))

    @Test
    def sequenceWither03(): Unit \ Assert =
        let xs: List[Option[Option[Int32]]] = Some(None) :: Nil;
        assertEq(expected = Some(Nil), sequenceWither(xs))

    @Test
    def sequenceWither04(): Unit \ Assert =
        let xs = Some(Some(1)) :: Nil;
        assertEq(expected = Some(1 :: Nil), sequenceWither(xs))

    @Test
    def sequenceWither05(): Unit \ Assert =
        let xs = Some(Some(1)) :: Some(Some(2)) :: Nil;
        assertEq(expected = Some(1 :: 2 :: Nil), sequenceWither(xs))

    @Test
    def sequenceWither06(): Unit \ Assert =
        let xs = Some(Some(1)) :: Some(None) :: Nil;
        assertEq(expected = Some(1 :: Nil), sequenceWither(xs))

    @Test
    def sequenceWither07(): Unit \ Assert =
        let xs = Some(Some(1)) :: Some(None) :: None :: Nil;
        assertEq(expected = None, sequenceWither(xs))

}

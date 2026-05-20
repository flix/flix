mod Test.Equality.Tag {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqTag01(): Unit \ Assert =
        assertEq(expected = (None: Option[Unit]), None)

    @Test
    def testEqTag02(): Unit \ Assert =
        assertEq(expected = Some(42), Some(42))

    @Test
    def testEqTag03(): Unit \ Assert =
        assertEq(expected = (Ok(42): Result[Unit, _]), Ok(42))

    @Test
    def testEqTag04(): Unit \ Assert =
        assertEq(expected = (Err(42): Result[_, Unit]), Err(42))

    @Test
    def testEqTag05(): Unit \ Assert =
        assertEq(expected = (Nil: List[Unit]), Nil)

    @Test
    def testEqTag06(): Unit \ Assert =
        assertEq(expected = (42 :: Nil), (42 :: Nil))

    @Test
    def testEqTag07(): Unit \ Assert =
        assertEq(expected = (Some(Ok(42)): Option[Result[Unit, _]]), Some(Ok(42)))

    @Test
    def testEqTag08(): Unit \ Assert =
        assertEq(expected = (Ok(Some(42)): Result[Unit, _]), Ok(Some(42)))

    @Test
    def testEqTag09(): Unit \ Assert =
        assertEq(expected = Some((Ok(1) :: Err(2) :: Nil)), Some((Ok(1) :: Err(2) :: Nil)))

    @Test
    def testNeqTag01(): Unit \ Assert =
        assertNeq(unexpected = None, Some(42))

    @Test
    def testNeqTag02(): Unit \ Assert =
        assertNeq(unexpected = Some(21), Some(42))

    @Test
    def testNeqTag03(): Unit \ Assert =
        assertNeq(unexpected = (Ok(21): Result[Unit, _]), Ok(42))

    @Test
    def testNeqTag04(): Unit \ Assert =
        assertNeq(unexpected = (Err(21): Result[_, Unit]), Err(42))

    @Test
    def testNeqTag05(): Unit \ Assert =
        assertNeq(unexpected = Nil, (42 :: Nil))

    @Test
    def testNeqTag06(): Unit \ Assert =
        assertNeq(unexpected = (21 :: Nil), (42 :: Nil))

    @Test
    def testNeqTag07(): Unit \ Assert =
        assertNeq(unexpected = Some(Ok(21)), Some(Err(42)))

    @Test
    def testNeqTag08(): Unit \ Assert =
        assertNeq(unexpected = (Ok(None): Result[Unit, Option[Int32]]), Ok(Some(42)))

    @Test
    def testNeqTag09(): Unit \ Assert =
        assertNeq(unexpected = Some((Ok(1) :: Err(2) :: Nil)), Some((Err(1) :: Ok(2) :: Nil)))

}

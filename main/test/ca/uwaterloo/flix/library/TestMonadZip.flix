mod TestMonadZip {

    use Assert.{assertEq}

    // Generally the functions from the `MonadZip` trait will be tested without
    // overloading in their respective modules. The tests here are basically
    // to test the trait instances can be invoked as expected.

    // Helper
    def necOf2(x: a, y: a) : Nec[a] = Nec.cons(x, Nec.singleton(y))


    /////////////////////////////////////////////////////////////////////////////
    // zipWithIdentity.Identity                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithIdentity01(): Unit \ Assert =
        assertEq(expected = Identity.Identity(10), MonadZip.zipWith(Int32.max, Identity.Identity(1), Identity.Identity(10)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAIdentity.Identity                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithAIdentity01(): Unit \ Assert =
        assertEq(expected = Identity.Identity(Identity.Identity(10)), MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Identity.Identity(1), Identity.Identity(10)))


    /////////////////////////////////////////////////////////////////////////////
    // zipIdentity.Identity                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipIdentity01(): Unit \ Assert =
        assertEq(expected = Identity.Identity((1, 10)), MonadZip.zip(Identity.Identity(1), Identity.Identity(10)))

    /////////////////////////////////////////////////////////////////////////////
    // unzipIdentity.Identity                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipIdentity01(): Unit \ Assert =
        assertEq(expected = (Identity.Identity(1), Identity.Identity(10)), MonadZip.unzip(Identity.Identity((1, 10))))


    /////////////////////////////////////////////////////////////////////////////
    // zipWithList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithList01(): Unit \ Assert =
        assertEq(expected = 10 :: 11 :: Nil, MonadZip.zipWith(Int32.max, 1 :: 2 :: Nil, 10 :: 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithAList01(): Unit \ Assert =
        assertEq(expected = Identity.Identity(10 :: 11 :: Nil), MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), 1 :: 2 :: Nil, 10 :: 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipList                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipList01(): Unit \ Assert =
        assertEq(expected = (1, 10) :: Nil, MonadZip.zip(1 :: 2 :: Nil, 10 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // unzipList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipList01(): Unit \ Assert =
        assertEq(expected = (1 :: 2 :: Nil, 10 :: 11 :: Nil), MonadZip.unzip((1, 10) :: (2, 11) :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // zipWithOption                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithOption01(): Unit \ Assert =
        assertEq(expected = None, MonadZip.zipWith(Int32.max, Some(1), None))

    @Test
    def zipWithOption02(): Unit \ Assert =
        assertEq(expected = Some(10), MonadZip.zipWith(Int32.max, Some(1), Some(10)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAOption                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithAOption01(): Unit \ Assert =
        assertEq(expected = Identity.Identity(None), MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Some(1), None))

    @Test
    def zipWithAOption02(): Unit \ Assert =
        assertEq(expected = Identity.Identity(Some(10)), MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Some(1), Some(10)))

    /////////////////////////////////////////////////////////////////////////////
    // zipOption                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipOption01(): Unit \ Assert =
        assertEq(expected = None, MonadZip.zip(Some(1), (None: Option[Int32])))

    @Test
    def zipOption02(): Unit \ Assert =
        assertEq(expected = Some((1, 10)), MonadZip.zip(Some(1), Some(10)))


    /////////////////////////////////////////////////////////////////////////////
    // unzipOption                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipOption01(): Unit \ Assert =
        assertEq(expected = (None, None), MonadZip.unzip((None: Option[(Int32, Int32)])))

    @Test
    def unzipOption02(): Unit \ Assert =
        assertEq(expected = (Some(1), Some(10)), MonadZip.unzip(Some((1, 10))))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithChain                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithChain01(): Unit \ Assert =
        assertEq(expected = List.toChain(10 :: 11 :: Nil), MonadZip.zipWith(Int32.max, List.toChain(1 :: 2 :: Nil), List.toChain(10 :: 11 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAChain                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithAChain01(): Unit \ Assert =
        assertEq(expected = Identity.Identity(List.toChain(10 :: 11 :: Nil)), MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), List.toChain(1 :: 2 :: Nil), List.toChain(10 :: 11 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipChain                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipChain01(): Unit \ Assert =
        assertEq(expected = Chain.singleton((1, 10)), MonadZip.zip(List.toChain(1 :: 2 :: Nil), Chain.singleton(10)))

    /////////////////////////////////////////////////////////////////////////////
    // unzipChain                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipChain01(): Unit \ Assert =
        assertEq(expected = (List.toChain(1 :: 2 :: Nil), List.toChain(10 :: 11 :: Nil)), MonadZip.unzip(List.toChain((1, 10) :: (2, 11) :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithNel                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithNel01(): Unit \ Assert =
        assertEq(expected = Nel.Nel(10, 11 :: Nil), MonadZip.zipWith(Int32.max, Nel.Nel(1, 2 :: Nil), Nel.Nel(10, 11 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithANel                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithANel01(): Unit \ Assert =
        assertEq(expected = Identity.Identity(Nel.Nel(10, 11 :: Nil)), MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Nel.Nel(1, 2 :: Nil), Nel.Nel(10, 11 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipNel                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipNel01(): Unit \ Assert =
        assertEq(expected = Nel.Nel((1, 10), Nil), MonadZip.zip(Nel.Nel(1, 2 :: Nil), Nel.Nel(10, Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // unzipNel                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipNel01(): Unit \ Assert =
        assertEq(expected = (Nel.Nel(1, 2 :: Nil), Nel.Nel(10, 11 :: Nil)), MonadZip.unzip(Nel.Nel((1, 10), (2, 11) :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithNec                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithNec01(): Unit \ Assert =
        assertEq(expected = necOf2(10, 11), MonadZip.zipWith(Int32.max, necOf2(1, 2), necOf2(10, 11)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithANec                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithANec01(): Unit \ Assert =
        assertEq(expected = Identity.Identity(necOf2(10, 11)), MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), necOf2(1, 2), necOf2(10, 11)))

    /////////////////////////////////////////////////////////////////////////////
    // zipNec                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipNec01(): Unit \ Assert =
        assertEq(expected = Nec.singleton((1, 10)), MonadZip.zip(necOf2(1, 2), Nec.singleton(10)))

    /////////////////////////////////////////////////////////////////////////////
    // unzipNec                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipNec01(): Unit \ Assert =
        assertEq(expected = (necOf2(1, 2), necOf2(10, 11)), MonadZip.unzip(necOf2((1, 10), (2, 11))))

}


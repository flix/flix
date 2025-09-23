mod TestApplicative {

    use Assert.{assertEq};

    use Applicative.{product, productLeft, productRight, product3, product4, product5}
    use Applicative.{<*>, <**>, *>, <*}

    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product01(): Unit \ Assert =
        assertEq(expected = None, product((None: Option[Int32]), Some(2)))

    @Test
    def product02(): Unit \ Assert =
        assertEq(expected = None, product(Some(1), (None: Option[Int32])))

    @Test
    def product03(): Unit \ Assert =
        assertEq(expected = Some((1, 2)), product(Some(1), Some(2)))

    @Test
    def product04(): Unit \ Assert =
        assertEq(expected = Nil, product((Nil: List[Int32]), 2 :: Nil))

    @Test
    def product05(): Unit \ Assert =
        assertEq(expected = Nil, product(1 :: Nil, (Nil: List[Int32])))

    @Test
    def product06(): Unit \ Assert =
        assertEq(expected = (1, 2) :: Nil, product(1 :: Nil, 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // productLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def productLeft01(): Unit \ Assert =
        assertEq(expected = None, productLeft((None: Option[Int32]), Some(2)))

    @Test
    def productLeft02(): Unit \ Assert =
        assertEq(expected = None, productLeft(Some(1), None))

    @Test
    def productLeft03(): Unit \ Assert =
        assertEq(expected = Some(1), productLeft(Some(1), Some(2)))

    @Test
    def productLeft04(): Unit \ Assert =
        assertEq(expected = Nil, productLeft((Nil: List[Int32]), 2 :: Nil))

    @Test
    def productLeft05(): Unit \ Assert =
        assertEq(expected = Nil, productLeft(1 :: Nil, Nil))

    @Test
    def productLeft06(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, productLeft(1 :: Nil, 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // productRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def productRight01(): Unit \ Assert =
        assertEq(expected = None, productRight(None, Some(2)))

    @Test
    def productRight02(): Unit \ Assert =
        assertEq(expected = None, productRight(Some(1), (None: Option[Int32])))

    @Test
    def productRight03(): Unit \ Assert =
        assertEq(expected = Some(2), productRight(Some(1), Some(2)))

    @Test
    def productRight04(): Unit \ Assert =
        assertEq(expected = Nil, productRight((Nil: List[Int32]), 2 :: Nil))

    @Test
    def productRight05(): Unit \ Assert =
        assertEq(expected = Nil, productRight(1 :: Nil, (Nil: List[Int32])))

    @Test
    def productRight06(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, productRight(1 :: Nil, 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // product3                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product301(): Unit \ Assert =
        assertEq(expected = None, product3((None: Option[Int32]), (None: Option[Int32]), (None: Option[Int32])))

    @Test
    def product302(): Unit \ Assert =
        assertEq(expected = None, product3((None: Option[Int32]), Some(2), Some(3)))

    @Test
    def product303(): Unit \ Assert =
        assertEq(expected = None, product3(Some(1), Some(2), (None: Option[Int32])))

    @Test
    def product304(): Unit \ Assert =
        assertEq(expected = Some((1, 2, 3)), product3(Some(1), Some(2), Some(3)))

    /////////////////////////////////////////////////////////////////////////////
    // product4                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product401(): Unit \ Assert =
        assertEq(expected = None, product4((None: Option[Int32]), (None: Option[Int32]), (None: Option[Int32]), (None: Option[Int32])))

    @Test
    def product402(): Unit \ Assert =
        assertEq(expected = None, product4((None: Option[Int32]), Some(2), Some(3), Some(4)))

    @Test
    def product403(): Unit \ Assert =
        assertEq(expected = None, product4(Some(1), Some(2), Some(3), (None: Option[Int32])))

    @Test
    def product404(): Unit \ Assert =
        assertEq(expected = Some((1, 2, 3, 4)), product4(Some(1), Some(2), Some(3), Some(4)))

    /////////////////////////////////////////////////////////////////////////////
    // product5                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product501(): Unit \ Assert =
        assertEq(expected = None, product5((None: Option[Int32]), (None: Option[Int32]), (None: Option[Int32]), (None: Option[Int32]), (None: Option[Int32])))

    @Test
    def product502(): Unit \ Assert =
        assertEq(expected = None, product5((None: Option[Int32]), Some(2), Some(3), Some(4), Some(5)))

    @Test
    def product503(): Unit \ Assert =
        assertEq(expected = None, product5(Some(1), Some(2), Some(3), Some(4), (None: Option[Int32])))

    @Test
    def product504(): Unit \ Assert =
        assertEq(expected = Some((1, 2, 3, 4, 5)), product5(Some(1), Some(2), Some(3), Some(4), Some(5)))

    /////////////////////////////////////////////////////////////////////////////
    // symAp (<*>)                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symAp01(): Unit \ Assert =
        assertEq(expected = None, (None: Option[Int32 -> Int32]) <*> (None: Option[Int32]))

    @Test
    def symAp02(): Unit \ Assert =
        assertEq(expected = None, Some(x -> x+1) <*> None)

    @Test
    def symAp03(): Unit \ Assert =
        assertEq(expected = Some(2), Some(x -> x+1) <*> Some(1))

    /////////////////////////////////////////////////////////////////////////////
    // symReversedAp (<**>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symReversedAp01(): Unit \ Assert =
        assertEq(expected = None, (None: Option[Int32]) <**> (None: Option[Int32 -> Int32]))

    @Test
    def symReversedAp02(): Unit \ Assert =
        assertEq(expected = None, None <**> Some(x -> x+1))

    @Test
    def symReversedAp03(): Unit \ Assert =
        assertEq(expected = Some(2), Some(1) <**> Some(x -> x+1))

    /////////////////////////////////////////////////////////////////////////////
    // symProductLeft (<*)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symProductLeft01(): Unit \ Assert =
        assertEq(expected = None, (None: Option[Int32]) <* Some(2))

    @Test
    def symProductLeft02(): Unit \ Assert =
        assertEq(expected = None, Some(1) <* None)

    @Test
    def symProductLeft03(): Unit \ Assert =
        assertEq(expected = Some(1), Some(1) <* Some(2))

    /////////////////////////////////////////////////////////////////////////////
    // symProductRight (*>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symProductRight01(): Unit \ Assert =
        assertEq(expected = None, None *> Some(2))

    @Test
    def symProductRight02(): Unit \ Assert =
        assertEq(expected = None, Some(1) *> (None: Option[Int32]))

    @Test
    def symProductRight03(): Unit \ Assert =
        assertEq(expected = Some(2), Some(1) *> Some(2))

}

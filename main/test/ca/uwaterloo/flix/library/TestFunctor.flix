mod TestFunctor {

    use Assert.{assertEq};
    use Functor.{map, ignore};
    use Functor.{<$>, <$$>, <$, $>};


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert =
        assertEq(expected = None, map(x -> x + 1, None))

    @Test
    def map02(): Unit \ Assert =
        assertEq(expected = Some(2), map(x -> x + 1, Some(1)))

    @Test
    def map03(): Unit \ Assert =
        assertEq(expected = Nil, map(x -> x + 1, Nil))

    @Test
    def map04(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, map(x -> x + 1, 1 :: Nil))

    @Test
    def map05(): Unit \ Assert =
        assertEq(expected = 2 :: 3 :: Nil, map(x -> x + 1, 1 :: 2 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert =
        assertEq(expected = None, Functor.replace(None, 'A'))

    @Test
    def replace02(): Unit \ Assert =
        assertEq(expected = Some('A'), Functor.replace(Some(1), 'A'))

    @Test
    def replace03(): Unit \ Assert =
        assertEq(expected = Nil, Functor.replace(Nil, 'A'))

    @Test
    def replace04(): Unit \ Assert =
        assertEq(expected = 'A' :: Nil, Functor.replace(1 :: Nil, 'A'))

    @Test
    def replace05(): Unit \ Assert =
        assertEq(expected = 'A' :: 'A' :: Nil, Functor.replace(1 :: 2 :: Nil, 'A'))


    /////////////////////////////////////////////////////////////////////////////
    // ignore                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ignore01(): Unit \ Assert =
        assertEq(expected = None, ignore(None))

    @Test
    def ignore02(): Unit \ Assert =
        assertEq(expected = Some(()), ignore(Some(1)))

    @Test
    def ignore03(): Unit \ Assert =
        assertEq(expected = Nil, ignore(Nil))

    @Test
    def ignore04(): Unit \ Assert =
        assertEq(expected = () :: Nil, ignore(1 :: Nil))

    @Test
    def ignore05(): Unit \ Assert =
        assertEq(expected = () :: () :: Nil, ignore(1 :: 2 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // symMap (<$>)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symMap01(): Unit \ Assert =
        assertEq(expected = None, (x -> x + 1) <$> None)

    @Test
    def symMap02(): Unit \ Assert =
        assertEq(expected = Some(2), (x -> x + 1) <$> Some(1))


    /////////////////////////////////////////////////////////////////////////////
    // symFlippedMap (<$$>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symFlippedMap01(): Unit \ Assert =
        assertEq(expected = None, None <$$> (x -> x + 1))

    @Test
    def symFlippedMap02(): Unit \ Assert =
        assertEq(expected = Some(2), Some(1) <$$> (x -> x + 1))


    /////////////////////////////////////////////////////////////////////////////
    // symAs (<$)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symAs01(): Unit \ Assert =
        assertEq(expected = None, () <$ None)

    @Test
    def symAs02(): Unit \ Assert =
        assertEq(expected = Some(()), () <$ Some(1))


    /////////////////////////////////////////////////////////////////////////////
    // symFlippedAs ($>)                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symFlippedAs01(): Unit \ Assert =
        assertEq(expected = None, None $> ())

    @Test
    def symFlippedAs02(): Unit \ Assert =
        assertEq(expected = Some(()), Some(1) $> ())

}

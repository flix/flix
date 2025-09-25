mod TestMonad {

    use Assert.{assertEq}
    use Monad.{flatten, kleisliLeft, kleisliRight}
    use Monad.{=<<, >>=, >=>, <=<}

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert =
        let x: Option[Option[Int32]] = None;
        assertEq(expected = None, flatten(x))

    @Test
    def flatten02(): Unit \ Assert =
        let x: Option[Option[Int32]] = Some(None);
        assertEq(expected = None, flatten(x))

    @Test
    def flatten03(): Unit \ Assert =
        let x: Option[Option[Int32]] = Some(Some(1));
        assertEq(expected = Some(1), flatten(x))

    /////////////////////////////////////////////////////////////////////////////
    // kleisliLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def kleisliLeft01(): Unit \ Assert =
        assertEq(expected = None, kleisliLeft(_ -> (None: Option[Int32]), x2 -> Some(ToString.toString(x2)), 1))

    @Test
    def kleisliLeft02(): Unit \ Assert =
        assertEq(expected = Some("2"), kleisliLeft(x1 -> Some(x1 + 1), x2 -> Some(ToString.toString(x2)), 1))

    /////////////////////////////////////////////////////////////////////////////
    // kleisliRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def kleisliRight01(): Unit \ Assert =
        assertEq(expected = None, kleisliRight(x1 -> Some(ToString.toString(x1)), _ -> (None: Option[Int32]), 1))

    @Test
    def kleisliRight02(): Unit \ Assert =
        assertEq(expected = Some("2"), kleisliRight(x1 -> Some(ToString.toString(x1)), x2 -> Some(x2 + 1), 1))

    /////////////////////////////////////////////////////////////////////////////
    // symFlatMap (=<<)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symFlatMap01(): Unit \ Assert =
        assertEq(expected = None, (x -> Some(x + 1)) =<< None)

    @Test
    def symFlatMap02(): Unit \ Assert =
        assertEq(expected = Some(2), (x -> Some(x + 1)) =<< Some(1))

    /////////////////////////////////////////////////////////////////////////////
    // symBind (>>=)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symBind01(): Unit \ Assert =
        assertEq(expected = None, None >>= (x -> Some(x + 1)))

    @Test
    def symBind02(): Unit \ Assert =
        assertEq(expected = Some(2), Some(1) >>= (x -> Some(x + 1)))

    /////////////////////////////////////////////////////////////////////////////
    // symKleisliLeft (>=>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symKleisliLeft01(): Unit \ Assert =
        let mf = (_ -> (None: Option[Int32])) >=> (x2 -> Some(ToString.toString(x2)));
        assertEq(expected = None, mf(1))

    @Test
    def symKleisliLeft02(): Unit \ Assert =
        let mf = (x1 -> Some(x1 + 1)) >=> (x2 -> Some(ToString.toString(x2)));
        assertEq(expected = Some("2"), mf(1))

    /////////////////////////////////////////////////////////////////////////////
    // symKleisliRight (<=<)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symKleisliRight01(): Unit \ Assert =
        let mf = (x1 -> Some(ToString.toString(x1))) <=< (_ -> (None: Option[Int32]));
        assertEq(expected = None, mf(1))

    @Test
    def symKleisliRight02(): Unit \ Assert =
        let mf = (x1 -> Some(ToString.toString(x1))) <=< (x2 -> Some(x2 + 1));
        assertEq(expected = Some("2"), mf(1))

}


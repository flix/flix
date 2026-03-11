mod TestCoerce {

    use Assert.assertEq;

    @Test
    pub def testCoerceBox01(): Unit \ Assert =
        assertEq(expected = 21, Coerce.coerce(Box.Box(21)))

    @Test
    pub def testCoerceBox02(): Unit \ Assert =
        assertEq(expected = 42, Coerce.coerce(Box.Box(42)))

    @Test
    pub def testCoercePolyBox01(): Unit \ Assert =
        assertEq(expected = true, Coerce.coerce(PolyBox.PolyBox(true)))

    @Test
    pub def testCoercePolyBox02(): Unit \ Assert =
        assertEq(expected = false, Coerce.coerce(PolyBox.PolyBox(false)))

    @Test
    pub def testCoercePolyBox03(): Unit \ Assert =
        assertEq(expected = 42, Coerce.coerce(PolyBox.PolyBox(42)))

    @Test
    pub def testCoercePolyBox04(): Unit \ Assert =
        assertEq(expected = "Hello World", Coerce.coerce(PolyBox.PolyBox("Hello World")))

    enum Box {
        case Box(Int32)
    }

    instance Coerce[Box] {
        type Out = Int32
        pub def coerce(x: Box): Int32 = match x {
            case Box.Box(v) => v
        }
    }

    enum PolyBox[a] {
        case PolyBox(a)
    }

    instance Coerce[PolyBox[a]] {
        type Out = a
        pub def coerce(x: PolyBox[a]): a = match x {
            case PolyBox.PolyBox(v) => v
        }
    }

}
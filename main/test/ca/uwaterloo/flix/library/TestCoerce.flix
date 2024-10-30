mod TestCoerce {

    @Test
    pub def testCoerceBox01(): Bool =
        Coerce.coerce(Box.Box(21)) == 21

    pub def testCoerceBox02(): Bool =
        Coerce.coerce(Box.Box(42)) == 42

    @Test
    pub def testCoercePolyBox01(): Bool =
        Coerce.coerce(PolyBox.PolyBox(true)) == true

    @Test
    pub def testCoercePolyBox02(): Bool =
        Coerce.coerce(PolyBox.PolyBox(false)) == false

    @Test
    pub def testCoercePolyBox03(): Bool =
        Coerce.coerce(PolyBox.PolyBox(42)) == 42

    @Test
    pub def testCoercePolyBox04(): Bool =
        Coerce.coerce(PolyBox.PolyBox("Hello World")) == "Hello World"

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

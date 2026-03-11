mod Test.Exp.Let.Match {

    use Assert.assertEq

    @Test
    def testLetMatch01(): Unit \ Assert =
        let (x, y) = (1, 2);
        assertEq(expected = true, x + y == 1 + 2)

    @Test
    def testLetMatch02(): Unit \ Assert =
        let (x, y, z) = (1, 2, 3);
        assertEq(expected = true, x + y + z == 1 + 2 + 3)

    @Test
    def testLetMatch03(): Unit \ Assert =
        let ((x, y), (z, w)) = ((1, 2), (3, 4));
        assertEq(expected = true, x + w + y + z == 1 + 2 + 3 + 4)

    @Test
    def testLetMatch04(): Unit \ Assert =
        let Celsius.Celsius(x) = Celsius.Celsius(42);
        assertEq(expected = true, x == 42)

    @Test
    def testLetMatch05(): Unit \ Assert =
        let Fahrenheit.Fahrenheit(x) = Fahrenheit.Fahrenheit(42);
        assertEq(expected = true, x == 42)

    @Test
    def testLetMatch06(): Unit \ Assert =
        let Fahrenheit.Fahrenheit(x) = Fahrenheit.Fahrenheit(42);
        assertEq(expected = true, x == 42)

    @Test
    def testLetMatch07(): Unit \ Assert =
        let Box.Box((x, y)) = Box.Box((1, 2));
        assertEq(expected = true, x + y == 1 + 2)

    @Test
    def testLetMatch08(): Unit \ Assert =
        let Box.Box((x, y, z)) = Box.Box((1, 2, 3));
        assertEq(expected = true, x + y + z == 1 + 2 + 3)

    @Test
    def testLetMatch09(): Unit \ Assert =
        let Box.Box(((x, y), (z, w))) = Box.Box(((1, 2), (3, 4)));
        assertEq(expected = true, x + y + z + w == 1 + 2 + 3 + 4)

    @Test
    def testLetMatch10(): Unit \ Assert =
        let Box.Box(Box.Box(x)) = Box.Box(Box.Box(42));
        assertEq(expected = true, x == 42)

    @Test
    def testLetMatch11(): Unit \ Assert =
        let Box.Box((Box.Box(x), Box.Box(y))) = Box.Box((Box.Box(1), Box.Box(2)));
        assertEq(expected = true, x + y == 1 + 2)

    @Test
    def testLetMatch12(): Unit \ Assert =
        let Box.Box((Box.Box((x, y)), Box.Box((z, w)))) = Box.Box((Box.Box((1, 2)), Box.Box((3, 4))));
        assertEq(expected = true, x + y + z + w == 1 + 2 + 3 + 4)

    @Test
    def testLetMatch13(): Unit \ Assert =
        let Box.Box(Celsius.Celsius(x)) = Box.Box(Celsius.Celsius(42));
        assertEq(expected = true, x == 42)

    enum Celsius {
        case Celsius(Int32)
    }

    enum Fahrenheit {
        case Fahrenheit(Int32)
    }

    enum Box[a] {
        case Box(a)
    }

    @Test
    def testLetMatch14(): Unit \ Assert =
        let { x } = { x = 1 };
        assertEq(expected = true, x == 1)

    @Test
    def testLetMatch15(): Unit \ Assert =
        let { x = x } = { x = 1 };
        assertEq(expected = true, x == 1)

    @Test
    def testLetMatch16(): Unit \ Assert =
        let { x | _ } = { x = 1, y = "" };
        assertEq(expected = true, x == 1)

    @Test
    def testLetMatch17(): Unit \ Assert =
        let { x, y } = { x = 1, y = "" };
        assertEq(expected = true, x == 1 and y == "")

    @Test
    def testLetMatch18(): Unit \ Assert =
        let { y, x } = { x = 1, y = "" };
        assertEq(expected = true, x == 1 and y == "")

    @Test
    def testLetMatch19(): Unit \ Assert =
        let { y, x = a, x} = { x = 1, y = "", x = false};
        assertEq(expected = true, (a == 1 and y == "") and x == false)

    @Test
    def testLetMatch20(): Unit \ Assert =
        let { y, x = a, x = { x }} = { x = 1, y = "", x = { x = true }};
        assertEq(expected = true, (a == 1 and y == "") and x)

    @Test
    def testLetMatch21(): Unit \ Assert =
        let { x = a, x = { x }, y } = { x = 1, y = "", x = { x = true }};
        assertEq(expected = true, (a == 1 and y == "") and x)

    @Test
    def testLetMatch22(): Unit \ Assert =
        let { x, x = { x = a }, y } = { x = 1, y = "", x = { x = true }};
        assertEq(expected = true, (x == 1 and y == "") and a)

    @Test
    def testLetMatch23(): Unit \ Assert =
        let { x = { x = { x } } | _ } = { x = { x = { x = 42 } } };
        assertEq(expected = true, x == 42)

    @Test
    def testLetMatch24(): Unit \ Assert =
        let { x = { x = { x } } | _ } = { x = { x = { x = 42 } }, y = "" };
        assertEq(expected = true, x == 42)

    @Test
    def testLetMatch25(): Unit \ Assert =
        let { x = { x = { x | _ } } | _ } = { x = { x = { x = 42, y = false } }, y = "" };
        assertEq(expected = true, x == 42)

}

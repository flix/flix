mod Test.Exp.Let.Match {

    @Test
    def testLetMatch01(): Bool =
        let (x, y) = (1, 2);
            x + y == 1 + 2

    @Test
    def testLetMatch02(): Bool =
        let (x, y, z) = (1, 2, 3);
            x + y + z == 1 + 2 + 3

    @Test
    def testLetMatch03(): Bool =
        let ((x, y), (z, w)) = ((1, 2), (3, 4));
            x + w + y + z == 1 + 2 + 3 + 4

    @Test
    def testLetMatch04(): Bool =
        let Celsius.Celsius(x) = Celsius.Celsius(42);
            x == 42

    @Test
    def testLetMatch05(): Bool =
        let Fahrenheit.Fahrenheit(x) = Fahrenheit.Fahrenheit(42);
            x == 42

    @Test
    def testLetMatch06(): Bool =
        let Fahrenheit.Fahrenheit(x) = Fahrenheit.Fahrenheit(42);
            x == 42

    @Test
    def testLetMatch07(): Bool =
        let Box.Box((x, y)) = Box.Box((1, 2));
            x + y == 1 + 2

    @Test
    def testLetMatch08(): Bool =
        let Box.Box((x, y, z)) = Box.Box((1, 2, 3));
            x + y + z == 1 + 2 + 3

    @Test
    def testLetMatch09(): Bool =
        let Box.Box((x, y), (z, w)) = Box.Box(((1, 2), (3, 4)));
            x + y + z + w == 1 + 2 + 3 + 4

    @Test
    def testLetMatch10(): Bool =
        let Box.Box(Box.Box(x)) = Box.Box(Box.Box(42));
            x == 42

    @Test
    def testLetMatch11(): Bool =
        let Box.Box((Box.Box(x), Box.Box(y))) = Box.Box((Box.Box(1), Box.Box(2)));
            x + y == 1 + 2

    @Test
    def testLetMatch12(): Bool =
        let Box.Box((Box.Box((x, y)), Box.Box((z, w)))) = Box.Box((Box.Box((1, 2)), Box.Box((3, 4))));
            x + y + z + w == 1 + 2 + 3 + 4

    @Test
    def testLetMatch13(): Bool =
        let Box.Box(Celsius.Celsius(x)) = Box.Box(Celsius.Celsius(42));
            x == 42

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
    def testLetMatch14(): Bool =
        let { x } = { x = 1 };
        x == 1

    @Test
    def testLetMatch15(): Bool =
        let { x = x } = { x = 1 };
        x == 1

    @Test
    def testLetMatch16(): Bool =
        let { x | _ } = { x = 1, y = "" };
        x == 1

    @Test
    def testLetMatch17(): Bool =
        let { x, y } = { x = 1, y = "" };
        x == 1 and y == ""

    @Test
    def testLetMatch18(): Bool =
        let { y, x } = { x = 1, y = "" };
        x == 1 and y == ""

    @Test
    def testLetMatch19(): Bool =
        let { y, x = a, x} = { x = 1, y = "", x = false};
        (a == 1 and y == "") and x == false

    @Test
    def testLetMatch20(): Bool =
        let { y, x = a, x = { x }} = { x = 1, y = "", x = { x = true }};
        (a == 1 and y == "") and x

    @Test
    def testLetMatch21(): Bool =
        let { x = a, x = { x }, y } = { x = 1, y = "", x = { x = true }};
        (a == 1 and y == "") and x

    @Test
    def testLetMatch22(): Bool =
        let { x, x = { x = a }, y } = { x = 1, y = "", x = { x = true }};
        (x == 1 and y == "") and a

    @Test
    def testLetMatch23(): Bool =
        let { x = { x = { x } } | _ } = { x = { x = { x = 42 } } };
        x == 42

    @Test
    def testLetMatch24(): Bool =
        let { x = { x = { x } } | _ } = { x = { x = { x = 42 } }, y = "" };
        x == 42

    @Test
    def testLetMatch25(): Bool =
        let { x = { x = { x | _ } } | _ } = { x = { x = { x = 42, y = false } }, y = "" };
        x == 42

}

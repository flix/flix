mod Test.Exp.Fixpoint.Inject {

    use Assert.assertEq;

    enum E[a] with Eq, Order, ToString {
        case E1(a)
        case E2(a, a)
    }

    @Test
    def inject01(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[Bool]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject02(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[Char]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject03(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[Float32]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject04(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[Float64]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject05(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[BigDecimal]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject06(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[Int8]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject07(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[Int16]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject08(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[Int32]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject09(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[Int64]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject10(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[BigInt]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject11(): Unit \ Assert = {
        let l = (Vector.empty(): Vector[String]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject12(): Unit \ Assert = {
        let l = Vector#{1, 2, 3};
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject13(): Unit \ Assert = {
        let l = Vector#{"1", "2", "3"};
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject14(): Unit \ Assert = {
        let l = Vector#{E.E1("1"), E.E1("2"), E.E1("3"), E.E2("1", "2")};
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        assertEq(expected = l, r)
    }

    @Test
    def inject15(): Unit \ Assert = {
        let l1 = Vector#{1, 2, 3};
        let l2 = Vector#{4, 5, 6};
        let pr = inject l1, l2 into A/1, B/1;
        let r1 = query pr select x from A(x);
        let r2 = query pr select x from B(x);
        assertEq(expected = l1, r1);
        assertEq(expected = l2, r2)
    }

    @Test
    def inject16(): Unit \ Assert = {
        let l1 = Vector#{1, 2, 3};
        let l2 = Vector#{'a', 'b', 'c'};
        let pr = inject l1, l2 into A/1, B/1;
        let r1 = query pr select x from A(x);
        let r2 = query pr select x from B(x);
        assertEq(expected = l1, r1);
        assertEq(expected = l2, r2)
    }

    @Test
    def inject17(): Unit \ Assert = {
        let l = Vector#{(1, 2), (2, 3), (3, 4)};
        let pr = inject l into A/2;
        let r = query pr select (x, y) from A(x, y);
        assertEq(expected = l, r)
    }

    @Test
    def inject18(): Unit \ Assert = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{4, 5, 6};
        let pr = inject l1, l2 into A/2, B/1;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select x from B(x);
        assertEq(expected = l1, r1);
        assertEq(expected = l2, r2)
    }

    @Test
    def inject19(): Unit \ Assert = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{'a', 'b', 'c'};
        let pr = inject l1, l2 into A/2, B/1;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select x from B(x);
        assertEq(expected = l1, r1);
        assertEq(expected = l2, r2)
    }

    @Test
    def inject20(): Unit \ Assert = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{('a', 4), ('b', 5), ('c', 6)};
        let pr = inject l1, l2 into A/2, B/2;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select (x, y) from B(x, y);
        assertEq(expected = l1, r1);
        assertEq(expected = l2, r2)
    }

    @Test
    def inject21(): Unit \ Assert = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{(4, 5, 6), (5, 6, 7), (6, 7, 8)};
        let pr = inject l1, l2 into A/2, B/3;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select (x, y, z) from B(x, y, z);
        assertEq(expected = l1, r1);
        assertEq(expected = l2, r2)
    }

    @Test
    def inject22(): Unit \ Assert = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{(4, 5, 6), (5, 6, 7), (6, 7, 8)};
        let l3 = Set#{1.0f32, 2.0f32, 3.0f32};
        let pr = inject l1, l2, l3 into A/2, B/3, C/1;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select (x, y, z) from B(x, y, z);
        let r3 = query pr select x from C(x) |> Vector.toSet;
        assertEq(expected = l1, r1);
        assertEq(expected = l2, r2);
        assertEq(expected = l3, r3)
    }

}

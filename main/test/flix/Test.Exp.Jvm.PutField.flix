mod Test.Exp.Jvm.PutField {
    import java.awt.Point

    @Test
    def testPutField01(): Bool \ IO =
        let p = new Point(42, 43);
        p.x = 100;
        p.x == 100

    @Test
    def testPutField02(): Bool \ IO =
        let p = new Point(42, 43);
        p.y = 200;
        p.y == 200

    @Test
    def testPutField03(): Bool \ IO =
        let f = x -> x.y = 100;
        let p1 = new Point(42, 43);
        let p2 = new Point(0, 0);
        f(p1);
        f(p2);
        p1.x == 42 and p2.x == 0 and p1.y == p1.y and p1.y == 100

    @Test
    def testPutField04(): Unit \ IO =
        new Point(42, 43).x = 1

    @Test
    def testPutField05(): Unit \ IO =
        new Point(42, 43).x=1

    @Test
    def testPutField06(): Unit \ IO =
        (new Point(42, 43)).x=1

    @Test
    def testPutField07(): Unit \ IO =
        { rp = { p = new Point(42, 43) } }#rp#p.y=100

    // TODO: Requires: `exp = exp` parsing, constraints and code gen. Current impl uses `expr.field = expr`.
    // @Test
    // def testPutField08(): Unit \ IO =
    //     (new Point(42, 43).x) = 1

}

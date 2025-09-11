mod Test.Exp.Jvm.GetField {
    import java.awt.Point

    @Test
    def testGetField01(): Bool \ IO =
        new Point(42, 43).x == 42

    @Test
    def testGetField02(): Bool \ IO =
        new Point(42, 43).y == 43

    @Test
    def testGetField03(): Bool \ IO =
        let f = x -> x.y;
        f(new Point(42, 43)) == f(new Point(-1, 43))

}

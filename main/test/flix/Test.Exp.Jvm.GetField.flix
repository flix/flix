mod Test.Exp.Jvm.GetField {
    import java.awt.Point

    use Assert.assertEq;

    @Test
    def testGetField01(): Unit \ Assert + IO =
        assertEq(expected = 42, new Point(42, 43).x)

    @Test
    def testGetField02(): Unit \ Assert + IO =
        assertEq(expected = 43, new Point(42, 43).y)

    @Test
    def testGetField03(): Unit \ Assert + IO =
        let f = x -> x.y;
        assertEq(expected = f(new Point(42, 43)), f(new Point(-1, 43)))

}

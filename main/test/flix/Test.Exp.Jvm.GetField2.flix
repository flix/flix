mod Test.Exp.Jvm.InvokeMethod {
    import java.awt.Point

    @test
    def testGetField2_01(): Bool \ IO =
        new Point(42, 43).x == 42

    @test
    def testGetField2_02(): Bool \ IO =
        new Point(42, 43).y == 43

    @test
    def testGetField2_03(): Bool \ IO =
        let f = x -> x.y;
        f(new Point(42, 43)) == f(new Point(-1, 43))

}

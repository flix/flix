mod Test.Exp.Jvm.InvokeMethod2 {

    import java.awt.Point

    @test
    def testGetField2_01(): Bool \ IO =
        new Point(42, 43).x == 42

}

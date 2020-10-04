namespace Test/Exp/Choose {

    @test
    def testChoose01(): Bool =
        let f = x -> {
            choose x {
                case Absent => 1
            }
        };
        f(Absent) == 1

}

namespace Test/Exp/Choose {

    @test
    def testChoose01(): Bool =
        let f = x -> {
            choose x {
                case Absent => "a"
            }
        };
        f(Absent) == "a"

    @test
    def testChoose02(): Bool =
        let f = x -> {
            choose x {
                case Present(x) => x
            }
        };
        f(Present("a")) == "a"

}

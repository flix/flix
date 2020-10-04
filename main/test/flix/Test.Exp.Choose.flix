namespace Test/Exp/Choose {

    @test
    def testChooseOne01(): Bool =
        let f = x -> {
            choose x {
                case Absent => "a"
            }
        };
        f(Absent) == "a"

    @test
    def testChooseOne02(): Bool =
        let f = x -> {
            choose x {
                case Present(x) => x
            }
        };
        f(Present("a")) == "a"

    @test
    def testChooseOne03(): Bool =
        let f = x -> {
            choose x {
                case Absent     => "a"
                case Present(_) => "b"

            }
        };
        f(Absent) == "a"

    @test
    def testChooseOne04(): Bool =
        let f = x -> {
            choose x {
                case Absent     => "a"
                case Present(x) => x

            }
        };
        f(Present("b")) == "b"

}

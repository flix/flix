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

    @test
    def testChooseTwo01(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent) => "a"
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwo02(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Present(x)) => x
            }
        };
        f(Absent, Present("a")) == "a"

    @test
    def testChooseTwo03(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Present(x), Absent) => x
            }
        };
        f(Present("a"), Absent) == "a"

    @test
    def testChooseTwo04(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Present(x), Present(y)) => x + y
            }
        };
        f(Present("a"), Present("b")) == "ab"

}

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
                case _ => "a"
            }
        };
        f(Absent) == "a"

    @test
    def testChooseOne04(): Bool =
        let f = x -> {
            choose x {
                case _ => "a"
            }
        };
        f(Present("a")) == "a"

    @test
    def testChooseOne05(): Bool =
        let f = x -> {
            choose x {
                case Absent     => "a"
                case Present(_) => "b"

            }
        };
        f(Absent) == "a"

    @test
    def testChooseOne06(): Bool =
        let f = x -> {
            choose x {
                case Absent     => "a"
                case Present(x) => x

            }
        };
        f(Present("b")) == "b"

    @test
    def testChooseOne07(): Bool =
        let f = x -> {
            choose x {
                case Absent     => "a"
                case _          => "b"

            }
        };
        f(Absent) == "a"

    @test
    def testChooseOne08(): Bool =
        let f = x -> {
            choose x {
                case Absent     => "a"
                case _          => "b"

            }
        };
        f(Present("x")) == "b"

    @test
    def testChooseOne09(): Bool =
        let f = x -> {
            choose x {
                case Present(x) => x
                case _          => "b"

            }
        };
        f(Absent) == "b"

    @test
    def testChooseOne10(): Bool =
        let f = x -> {
            choose x {
                case Present(x) => x
                case _          => "b"

            }
        };
        f(Present("a")) == "a"

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

    @test
    def testChooseTwo05(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Present(x), _) => x
            }
        };
        f(Present("a"), Absent) == "a"

    @test
    def testChooseTwo06(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Present(x), _) => x
            }
        };
        f(Present("a"), Present("b")) == "a"

    @test
    def testChooseTwo07(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (_, Present(y)) => y
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwo08(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (_, Present(y)) => y
            }
        };
        f(Present("a"), Present("b")) == "b"

    @test
    def testChooseTwo10(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwo11(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Absent, Present("y")) == "a"

    @test
    def testChooseTwo12(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Present("x"), Absent) == "a"

    @test
    def testChooseTwo13(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Present("x"), Present("y")) == "a"

    @test
    def testChooseTwoWithTwoCases01(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(x)) => x
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwoWithTwoCases02(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(x)) => x
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwoWithTwoCases03(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Present(x), Absent) => x
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwoWithTwoCases04(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Present(x), Absent) => x
            }
        };
        f(Present("b"), Absent) == "b"

    @test
    def testChooseTwoWithTwoCases05(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent)         => "a"
                case (Present(x), Present(y)) => x + y
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwoWithTwoCases06(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent)         => "a"
                case (Present(x), Present(y)) => x + y
            }
        };
        f(Present("a"), Present("b")) == "ab"

    @test
    def testChooseTwoWithTwoCases07(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Present(y)) => y
                case (Present(x), Absent) => x
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwoWithTwoCases08(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Present(y)) => y
                case (Present(x), Absent) => x
            }
        };
        f(Present("a"), Absent) == "a"

    @test
    def testChooseTwoWithTwoCases09(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (_, Present(y)) => y
                case (Present(x), _) => x
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwoWithTwoCases10(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (_, Present(y)) => y
                case (Present(x), _) => x
            }
        };
        f(Present("a"), Absent) == "a"

    @test
    def testChooseTwoWithTwoCases11(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (_, Present(y)) => y
                case (Present(x), _) => x
            }
        };
        f(Present("a"), Present("b")) == "b"

    @test
    def testChooseTwoWithThreeCases01(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(y)) => y
                case (Present(x), Absent) => x
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwoWithThreeCases02(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(y)) => y
                case (Present(x), Absent) => x
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwoWithThreeCases03(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(y)) => y
                case (Present(x), Absent) => x
            }
        };
        f(Present("b"), Absent) == "b"

    @test
    def testChooseTwoWithThreeCases04(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Present(y)) => y
                case (Present(x), Absent) => x
                case (_, _)               => "c"
            }
        };
        f(Absent, Absent) == "c"

    @test
    def testChooseTwoWithThreeCases05(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Present(y)) => y
                case (Present(x), Absent) => x
                case (_, _)               => "c"
            }
        };
        f(Absent, Present("a")) == "a"

    @test
    def testChooseTwoWithThreeCases06(): Bool =
        let f = (x, y) -> {
            choose (x, y) {
                case (Absent, Present(y)) => y
                case (Present(x), Absent) => x
                case (_, _)               => "c"
            }
        };
        f(Present("a"), Absent) == "a"



}

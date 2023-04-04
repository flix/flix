mod Test.Exp.Choose {
    use Choice.{Present, Absent}

    @test
    def testChooseOne01(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent => "a"
            }
        };
        f(Absent) == "a"

    @test
    def testChooseOne02(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(r) => r
            }
        };
        f(Present("a")) == "a"

    @test
    def testChooseOne03(): Bool =
        let f = x -> {
            relational_choose x {
                case _ => "a"
            }
        };
        f(Absent) == "a"

    @test
    def testChooseOne04(): Bool =
        let f = x -> {
            relational_choose x {
                case _ => "a"
            }
        };
        f(Present("a")) == "a"

    @test
    def testChooseOne05(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case Present(_) => "b"

            }
        };
        f(Absent) == "a"

    @test
    def testChooseOne06(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case Present(r) => r

            }
        };
        f(Present("b")) == "b"

    @test
    def testChooseOne07(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case _          => "b"

            }
        };
        f(Absent) == "a"

    @test
    def testChooseOne08(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case _          => "b"

            }
        };
        f(Present("x")) == "b"

    @test
    def testChooseOne09(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(r) => r
                case _          => "b"

            }
        };
        f(Absent) == "b"

    @test
    def testChooseOne10(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(r) => r
                case _          => "b"

            }
        };
        f(Present("a")) == "a"

    @test
    def testChooseTwo01(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent) => "a"
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwo02(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
            }
        };
        f(Absent, Present("a")) == "a"

    @test
    def testChooseTwo03(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r), Absent) => r
            }
        };
        f(Present("a"), Absent) == "a"

    @test
    def testChooseTwo04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r1), Present(r2)) => r1 + r2
            }
        };
        f(Present("a"), Present("b")) == "ab"

    @test
    def testChooseTwo05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Absent) == "a"

    @test
    def testChooseTwo06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Present("b")) == "a"

    @test
    def testChooseTwo07(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwo08(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
            }
        };
        f(Present("a"), Present("b")) == "b"

    @test
    def testChooseTwo10(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwo11(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Absent, Present("y")) == "a"

    @test
    def testChooseTwo12(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Present("x"), Absent) == "a"

    @test
    def testChooseTwo13(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Present("x"), Present("y")) == "a"

    @test
    def testChooseTwoWithTwoCases01(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwoWithTwoCases02(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwoWithTwoCases03(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwoWithTwoCases04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Present(r), Absent) => r
            }
        };
        f(Present("b"), Absent) == "b"

    @test
    def testChooseTwoWithTwoCases05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => "a"
                case (Present(r1), Present(r2)) => r1 + r2
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwoWithTwoCases06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => "a"
                case (Present(r1), Present(r2)) => r1 + r2
            }
        };
        f(Present("a"), Present("b")) == "ab"

    @test
    def testChooseTwoWithTwoCases07(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwoWithTwoCases08(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Present("a"), Absent) == "a"

    @test
    def testChooseTwoWithTwoCases09(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
                case (Present(r), _) => r
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwoWithTwoCases10(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Absent) == "a"

    @test
    def testChooseTwoWithTwoCases11(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Present("b")) == "b"

    @test
    def testChooseTwoWithThreeCases01(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Absent) == "a"

    @test
    def testChooseTwoWithThreeCases02(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Present("b")) == "b"

    @test
    def testChooseTwoWithThreeCases03(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Present("b"), Absent) == "b"

    @test
    def testChooseTwoWithThreeCases04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
                case (_, _)               => "c"
            }
        };
        f(Absent, Absent) == "c"

    @test
    def testChooseTwoWithThreeCases05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
                case (_, _)               => "c"
            }
        };
        f(Absent, Present("a")) == "a"

    @test
    def testChooseTwoWithThreeCases06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
                case (_, _)               => "c"
            }
        };
        f(Present("a"), Absent) == "a"

    @test
    def testChooseThreeWithTwoCases01(): Bool =
        let f = (x, y, z) -> {
            relational_choose (x, y, z) {
                case (Absent, Absent, Absent)         => 1
                case (Present(_), Absent, Present(_)) => 2
            }
        };
        f(Absent, Absent, Absent) == 1

    @test
    def testChooseThreeWithTwoCases02(): Bool =
        let f = (x, y, z) -> {
            relational_choose (x, y, z) {
                case (Absent, Absent, Absent)         => 1
                case (Present(_), Absent, Present(_)) => 2
            }
        };
        f(Present(123), Absent, Present(456)) == 2

    @test
    def testChooseIf01(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => 1
                case _          => 2
            }
        };
        f(if (true) Absent else Present(123)) == 1

    @test
    def testChooseIf02(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(_) => 1
                case _          => 2
            }
        };
        f(if (true) Absent else Present(123)) == 2

    @test
    def testChooseIf03(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => 1
                case Present(_) => 2
            }
        };
        f(if (true) Absent else Present(123)) == 1

    @test
    def testChooseIf04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(if (true) Absent else Present(123), Absent) == 1

    @test
    def testChooseIf05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(if (true) Absent else Present(123), Present(456)) == 3

    @test
    def testChooseIf06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(Absent, if (true) Absent else Present(123)) == 1

    @test
    def testChooseIf07(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(Present(123), if (true) Absent else Present(456)) == 2

    @test
    def testChooseIf08(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(if (true) Absent else Present(123), if (true) Absent else Present(456)) == 1

    @test
    def testChooseIf09(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
            }
        };
        f(if (true) Absent else Present(123), Absent) == 1

    @test
    def testChooseIf10(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(_))       => 1
                case (Present(_), Present(_))   => 2
            }
        };
        f(if (true) Absent else Present(123), Present(456)) == 1

    @test
    def testChooseIf11(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => 1
                case (Absent, Present(_)) => 2
            }
        };
        f(Absent, if (true) Absent else Present(123)) == 1

    @test
    def testChooseIf12(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(_), Absent)     => 1
                case (Present(_), Present(_)) => 2
            }
        };
        f(Present(123), if (true) Absent else Present(456)) == 1

    @test
    def testChooseIf13(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(Absent, if (true) Absent else Present(123)) == 1

    @test
    def testChooseIf14(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(Present(123), if (true) Absent else Present(456)) == 1

    @test
    def testChooseIf15(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(if (true) Absent else Present(123), Absent) == 1

    @test
    def testChooseIf16(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(if (true) Absent else Present(123), Present(123)) == 2

    @test
    def testChooseIf17(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(if (true) Absent else Present(123),  if (true) Absent else Present(456)) == 1

}

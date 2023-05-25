mod Test.Exp.Choose {
    use Choice.{Present, Absent}

    def testChooseOne01(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent => "a"
            }
        };
        f(Absent) == "a"

    def testChooseOne02(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(r) => r
            }
        };
        f(Present("a")) == "a"

    def testChooseOne03(): Bool =
        let f = x -> {
            relational_choose x {
                case _ => "a"
            }
        };
        f(Absent) == "a"

    def testChooseOne04(): Bool =
        let f = x -> {
            relational_choose x {
                case _ => "a"
            }
        };
        f(Present("a")) == "a"

    def testChooseOne05(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case Present(_) => "b"

            }
        };
        f(Absent) == "a"

    def testChooseOne06(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case Present(r) => r

            }
        };
        f(Present("b")) == "b"

    def testChooseOne07(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case _          => "b"

            }
        };
        f(Absent) == "a"

    def testChooseOne08(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case _          => "b"

            }
        };
        f(Present("x")) == "b"

    def testChooseOne09(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(r) => r
                case _          => "b"

            }
        };
        f(Absent) == "b"

    def testChooseOne10(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(r) => r
                case _          => "b"

            }
        };
        f(Present("a")) == "a"

    def testChooseTwo01(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent) => "a"
            }
        };
        f(Absent, Absent) == "a"

    def testChooseTwo02(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
            }
        };
        f(Absent, Present("a")) == "a"

    def testChooseTwo03(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r), Absent) => r
            }
        };
        f(Present("a"), Absent) == "a"

    def testChooseTwo04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r1), Present(r2)) => r1 + r2
            }
        };
        f(Present("a"), Present("b")) == "ab"

    def testChooseTwo05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Absent) == "a"

    def testChooseTwo06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Present("b")) == "a"

    def testChooseTwo07(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
            }
        };
        f(Absent, Present("b")) == "b"

    def testChooseTwo08(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
            }
        };
        f(Present("a"), Present("b")) == "b"

    def testChooseTwo10(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Absent, Absent) == "a"

    def testChooseTwo11(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Absent, Present("y")) == "a"

    def testChooseTwo12(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Present("x"), Absent) == "a"

    def testChooseTwo13(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Present("x"), Present("y")) == "a"

    def testChooseTwoWithTwoCases01(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
            }
        };
        f(Absent, Absent) == "a"

    def testChooseTwoWithTwoCases02(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
            }
        };
        f(Absent, Present("b")) == "b"

    def testChooseTwoWithTwoCases03(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Absent) == "a"

    def testChooseTwoWithTwoCases04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Present(r), Absent) => r
            }
        };
        f(Present("b"), Absent) == "b"

    def testChooseTwoWithTwoCases05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => "a"
                case (Present(r1), Present(r2)) => r1 + r2
            }
        };
        f(Absent, Absent) == "a"

    def testChooseTwoWithTwoCases06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => "a"
                case (Present(r1), Present(r2)) => r1 + r2
            }
        };
        f(Present("a"), Present("b")) == "ab"

    def testChooseTwoWithTwoCases07(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Present("b")) == "b"

    def testChooseTwoWithTwoCases08(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Present("a"), Absent) == "a"

    def testChooseTwoWithTwoCases09(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
                case (Present(r), _) => r
            }
        };
        f(Absent, Present("b")) == "b"

    def testChooseTwoWithTwoCases10(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Absent) == "a"

    def testChooseTwoWithTwoCases11(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Present("b")) == "b"

    def testChooseTwoWithThreeCases01(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Absent) == "a"

    def testChooseTwoWithThreeCases02(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Present("b")) == "b"

    def testChooseTwoWithThreeCases03(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Present("b"), Absent) == "b"

    def testChooseTwoWithThreeCases04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
                case (_, _)               => "c"
            }
        };
        f(Absent, Absent) == "c"

    def testChooseTwoWithThreeCases05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
                case (_, _)               => "c"
            }
        };
        f(Absent, Present("a")) == "a"

    def testChooseTwoWithThreeCases06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
                case (_, _)               => "c"
            }
        };
        f(Present("a"), Absent) == "a"

    def testChooseThreeWithTwoCases01(): Bool =
        let f = (x, y, z) -> {
            relational_choose (x, y, z) {
                case (Absent, Absent, Absent)         => 1
                case (Present(_), Absent, Present(_)) => 2
            }
        };
        f(Absent, Absent, Absent) == 1

    def testChooseThreeWithTwoCases02(): Bool =
        let f = (x, y, z) -> {
            relational_choose (x, y, z) {
                case (Absent, Absent, Absent)         => 1
                case (Present(_), Absent, Present(_)) => 2
            }
        };
        f(Present(123), Absent, Present(456)) == 2

    def testChooseIf01(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => 1
                case _          => 2
            }
        };
        f(if (true) Absent else Present(123)) == 1

    def testChooseIf02(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(_) => 1
                case _          => 2
            }
        };
        f(if (true) Absent else Present(123)) == 2

    def testChooseIf03(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => 1
                case Present(_) => 2
            }
        };
        f(if (true) Absent else Present(123)) == 1

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

    def testChooseIf09(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
            }
        };
        f(if (true) Absent else Present(123), Absent) == 1

    def testChooseIf10(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(_))       => 1
                case (Present(_), Present(_))   => 2
            }
        };
        f(if (true) Absent else Present(123), Present(456)) == 1

    def testChooseIf11(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => 1
                case (Absent, Present(_)) => 2
            }
        };
        f(Absent, if (true) Absent else Present(123)) == 1

    def testChooseIf12(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(_), Absent)     => 1
                case (Present(_), Present(_)) => 2
            }
        };
        f(Present(123), if (true) Absent else Present(456)) == 1

    def testChooseIf13(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(Absent, if (true) Absent else Present(123)) == 1

    def testChooseIf14(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(Present(123), if (true) Absent else Present(456)) == 1

    def testChooseIf15(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(if (true) Absent else Present(123), Absent) == 1

    def testChooseIf16(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(if (true) Absent else Present(123), Present(123)) == 2

    def testChooseIf17(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(if (true) Absent else Present(123),  if (true) Absent else Present(456)) == 1

}

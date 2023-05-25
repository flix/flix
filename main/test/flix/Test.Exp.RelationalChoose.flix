/// Code generation has been disabled for relational choice
mod Test.Exp.Choose {
    use Choice.{Present, Absent}

    pub def testChooseOne01(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent => "a"
            }
        };
        f(Absent) == "a"

    pub def testChooseOne02(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(r) => r
            }
        };
        f(Present("a")) == "a"

    pub def testChooseOne03(): Bool =
        let f = x -> {
            relational_choose x {
                case _ => "a"
            }
        };
        f(Absent) == "a"

    pub def testChooseOne04(): Bool =
        let f = x -> {
            relational_choose x {
                case _ => "a"
            }
        };
        f(Present("a")) == "a"

    pub def testChooseOne05(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case Present(_) => "b"

            }
        };
        f(Absent) == "a"

    pub def testChooseOne06(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case Present(r) => r

            }
        };
        f(Present("b")) == "b"

    pub def testChooseOne07(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case _          => "b"

            }
        };
        f(Absent) == "a"

    pub def testChooseOne08(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => "a"
                case _          => "b"

            }
        };
        f(Present("x")) == "b"

    pub def testChooseOne09(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(r) => r
                case _          => "b"

            }
        };
        f(Absent) == "b"

    pub def testChooseOne10(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(r) => r
                case _          => "b"

            }
        };
        f(Present("a")) == "a"

    pub def testChooseTwo01(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent) => "a"
            }
        };
        f(Absent, Absent) == "a"

    pub def testChooseTwo02(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
            }
        };
        f(Absent, Present("a")) == "a"

    pub def testChooseTwo03(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r), Absent) => r
            }
        };
        f(Present("a"), Absent) == "a"

    pub def testChooseTwo04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r1), Present(r2)) => r1 + r2
            }
        };
        f(Present("a"), Present("b")) == "ab"

    pub def testChooseTwo05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Absent) == "a"

    pub def testChooseTwo06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Present("b")) == "a"

    pub def testChooseTwo07(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
            }
        };
        f(Absent, Present("b")) == "b"

    pub def testChooseTwo08(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
            }
        };
        f(Present("a"), Present("b")) == "b"

    pub def testChooseTwo10(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Absent, Absent) == "a"

    pub def testChooseTwo11(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Absent, Present("y")) == "a"

    pub def testChooseTwo12(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Present("x"), Absent) == "a"

    pub def testChooseTwo13(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, _) => "a"
            }
        };
        f(Present("x"), Present("y")) == "a"

    pub def testChooseTwoWithTwoCases01(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
            }
        };
        f(Absent, Absent) == "a"

    pub def testChooseTwoWithTwoCases02(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
            }
        };
        f(Absent, Present("b")) == "b"

    pub def testChooseTwoWithTwoCases03(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Absent) == "a"

    pub def testChooseTwoWithTwoCases04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Present(r), Absent) => r
            }
        };
        f(Present("b"), Absent) == "b"

    pub def testChooseTwoWithTwoCases05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => "a"
                case (Present(r1), Present(r2)) => r1 + r2
            }
        };
        f(Absent, Absent) == "a"

    pub def testChooseTwoWithTwoCases06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => "a"
                case (Present(r1), Present(r2)) => r1 + r2
            }
        };
        f(Present("a"), Present("b")) == "ab"

    pub def testChooseTwoWithTwoCases07(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Present("b")) == "b"

    pub def testChooseTwoWithTwoCases08(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Present("a"), Absent) == "a"

    pub def testChooseTwoWithTwoCases09(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
                case (Present(r), _) => r
            }
        };
        f(Absent, Present("b")) == "b"

    pub def testChooseTwoWithTwoCases10(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Absent) == "a"

    pub def testChooseTwoWithTwoCases11(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Present(r)) => r
                case (Present(r), _) => r
            }
        };
        f(Present("a"), Present("b")) == "b"

    pub def testChooseTwoWithThreeCases01(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Absent) == "a"

    pub def testChooseTwoWithThreeCases02(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Absent, Present("b")) == "b"

    pub def testChooseTwoWithThreeCases03(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => "a"
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
            }
        };
        f(Present("b"), Absent) == "b"

    pub def testChooseTwoWithThreeCases04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
                case (_, _)               => "c"
            }
        };
        f(Absent, Absent) == "c"

    pub def testChooseTwoWithThreeCases05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
                case (_, _)               => "c"
            }
        };
        f(Absent, Present("a")) == "a"

    pub def testChooseTwoWithThreeCases06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(r)) => r
                case (Present(r), Absent) => r
                case (_, _)               => "c"
            }
        };
        f(Present("a"), Absent) == "a"

    pub def testChooseThreeWithTwoCases01(): Bool =
        let f = (x, y, z) -> {
            relational_choose (x, y, z) {
                case (Absent, Absent, Absent)         => 1
                case (Present(_), Absent, Present(_)) => 2
            }
        };
        f(Absent, Absent, Absent) == 1

    pub def testChooseThreeWithTwoCases02(): Bool =
        let f = (x, y, z) -> {
            relational_choose (x, y, z) {
                case (Absent, Absent, Absent)         => 1
                case (Present(_), Absent, Present(_)) => 2
            }
        };
        f(Present(123), Absent, Present(456)) == 2

    pub def testChooseIf01(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => 1
                case _          => 2
            }
        };
        f(if (true) Absent else Present(123)) == 1

    pub def testChooseIf02(): Bool =
        let f = x -> {
            relational_choose x {
                case Present(_) => 1
                case _          => 2
            }
        };
        f(if (true) Absent else Present(123)) == 2

    pub def testChooseIf03(): Bool =
        let f = x -> {
            relational_choose x {
                case Absent     => 1
                case Present(_) => 2
            }
        };
        f(if (true) Absent else Present(123)) == 1

    pub def testChooseIf04(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(if (true) Absent else Present(123), Absent) == 1

    pub def testChooseIf05(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(if (true) Absent else Present(123), Present(456)) == 3

    pub def testChooseIf06(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(Absent, if (true) Absent else Present(123)) == 1

    pub def testChooseIf07(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(Present(123), if (true) Absent else Present(456)) == 2

    pub def testChooseIf08(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
                case (Absent, Present(_))       => 3
                case (Present(_), Present(_))   => 4
            }
        };
        f(if (true) Absent else Present(123), if (true) Absent else Present(456)) == 1

    pub def testChooseIf09(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)           => 1
                case (Present(_), Absent)       => 2
            }
        };
        f(if (true) Absent else Present(123), Absent) == 1

    pub def testChooseIf10(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Present(_))       => 1
                case (Present(_), Present(_))   => 2
            }
        };
        f(if (true) Absent else Present(123), Present(456)) == 1

    pub def testChooseIf11(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Absent, Absent)     => 1
                case (Absent, Present(_)) => 2
            }
        };
        f(Absent, if (true) Absent else Present(123)) == 1

    pub def testChooseIf12(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (Present(_), Absent)     => 1
                case (Present(_), Present(_)) => 2
            }
        };
        f(Present(123), if (true) Absent else Present(456)) == 1

    pub def testChooseIf13(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(Absent, if (true) Absent else Present(123)) == 1

    pub def testChooseIf14(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(Present(123), if (true) Absent else Present(456)) == 1

    pub def testChooseIf15(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(if (true) Absent else Present(123), Absent) == 1

    pub def testChooseIf16(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(if (true) Absent else Present(123), Present(123)) == 2

    pub def testChooseIf17(): Bool =
        let f = (x, y) -> {
            relational_choose (x, y) {
                case (_, Absent)        => 1
                case (_, Present(_))    => 2
            }
        };
        f(if (true) Absent else Present(123),  if (true) Absent else Present(456)) == 1

}

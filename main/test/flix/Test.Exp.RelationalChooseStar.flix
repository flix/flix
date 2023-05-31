/// Code generation has been disabled for relational choice
mod Test.Exp.ChooseStar {
    use Choice.{Absent, Present}

    pub def testChooseStarOne01(): Bool =
        let f = x -> {
            relational_choose* x {
                case Absent     => Absent
                case Present(v) => Present(v)
            }
        };
        let isAbsent = x -> relational_choose x {
            case Absent => true
        };
        f(Absent) |> isAbsent

    pub def testChooseStarOne02(): Bool =
        let f = x -> {
            relational_choose* x {
                case Absent     => Absent
                case Present(v) => Present(v)
            }
        };
        let isPresent = x -> relational_choose x {
            case Present(_) => true
        };
        f(Present(123)) |> isPresent

    pub def testChooseStarOne03(): Bool =
        let f = x -> {
            relational_choose* x {
                case Absent     => Present(123)
                case Present(_) => Absent
            }
        };
        let isAbsent = x -> relational_choose x {
            case Absent => true
        };
        f(Present(123)) |> isAbsent

    pub def testChooseStarOne04(): Bool =
        let f = x -> {
            relational_choose* x {
                case Absent     => Present(123)
                case Present(_) => Absent
            }
        };
        let isPresent = x -> relational_choose x {
            case Present(_) => true
        };
        f(Absent) |> isPresent

    pub def testChooseStarOTwo01(): Bool =
        let f = (x, y) -> {
            relational_choose* (x, y) {
                case (Absent, Absent)         => Absent
                case (Present(_), Present(_)) => Present(123)
            }
        };
        let isAbsent = x -> relational_choose x {
            case Absent => true
        };
        f(Absent, Absent) |> isAbsent

    pub def testChooseStarOTwo02(): Bool =
        let f = (x, y) -> {
            relational_choose* (x, y) {
                case (Absent, Absent)         => Absent
                case (Present(_), Present(_)) => Present(123)
            }
        };
        let isPresent = x -> relational_choose x {
            case Present(_) => true
        };
        f(Present(123), Present(456)) |> isPresent

    pub def testChooseStarOTwo03(): Bool =
        let f = (x, y) -> {
            relational_choose* (x, y) {
                case (Absent, Absent)         => Absent
                case (Absent, Present(_))     => Present(123)
                case (Present(_), Absent)     => Absent
                case (Present(_), Present(_)) => Present(123)
            }
        };
        let isPresent = x -> relational_choose x {
            case Present(_) => true
        };
        f(Absent, Present(123)) |> isPresent

    pub def testChooseStarOTwo04(): Bool =
        let f = (x, y) -> {
            relational_choose* (x, y) {
                case (Absent, Absent)         => Absent
                case (Absent, Present(_))     => Present(123)
                case (Present(_), Absent)     => Absent
                case (Present(_), Present(_)) => Present(123)
            }
        };
        let isPresent = x -> relational_choose x {
            case Present(_) => true
        };
        f(Present(123), Present(456)) |> isPresent

    pub def testChooseStarLub01(): Bool =
        let f = x -> {
            relational_choose* x {
                case Absent     => Present(123)
                case Present(_) => Present(123)
            }
        };
        let isPresent = x -> relational_choose x {
            case Present(_) => true
        };
        f(if (true) Absent else Present(42)) |> isPresent

    pub def testChooseStarLub02(): Bool =
        let f = x -> {
            relational_choose* x {
                case Absent     => Absent
                case Present(_) => Absent
            }
        };
        let isAbsent = x -> relational_choose x {
            case Absent => true
        };
        f(if (true) Absent else Present(42)) |> isAbsent

    pub def testMap01(): Bool =
        let map = (f, x) -> {
            relational_choose* x {
                case Absent     => Absent
                case Present(v) => Present(f(v))
            }
        };
        let isPresent = x -> relational_choose x {
            case Present(_) => true
        };
        map(x -> x * 2, Present(123)) |> isPresent

    pub def testMap02(): Bool =
        let map2 = (f, x, y) -> {
            relational_choose* (x, y) {
                case (Absent, Absent)           => Absent
                case (Absent, Present(_))       => Absent
                case (Present(_), Absent)       => Absent
                case (Present(v1), Present(v2)) => Present(f(v1, v2))
            }
        };
        let isPresent = x -> relational_choose x {
            case Present(_) => true
        };
        map2((x, y) -> x * y, Present(123), Present(456)) |> isPresent

    pub def testFlatMap01(): Bool =
        let flatMap = (f, x) -> {
            relational_choose* x {
                case Absent     => Absent
                case Present(v) => f(v)
            }
        };
        let isAbsent = x -> relational_choose x {
            case Absent => true
        };
        flatMap(x -> Present(x * 2), Absent) |> isAbsent

    pub def testFlatMap02(): Bool =
        let flatMap = (f, x) -> {
            relational_choose* x {
                case Absent     => Absent
                case Present(v) => f(v)
            }
        };
        let isPresent = x -> relational_choose x {
            case Present(_) => true
        };
        flatMap(x -> Present(x * 2), Present(123)) |> isPresent

    pub def testFlatMap03(): Bool =
        let flatMap = (f, x) -> {
            relational_choose* x {
                case Absent     => Absent
                case Present(v) => f(v)
            }
        };
        let isAbsent = x -> relational_choose x {
            case Absent => true
        };
        flatMap(_x -> Absent, Absent) |> isAbsent

    pub def testFlatMap04(): Bool =
        let flatMap = (f, x) -> {
            relational_choose* x {
                case Absent     => Absent
                case Present(v) => f(v)
            }
        };
        let isAbsent = x -> relational_choose x {
            case Absent => true
        };
        flatMap(_x -> Absent, Present(123)) |> isAbsent

}

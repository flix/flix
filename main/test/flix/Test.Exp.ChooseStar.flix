namespace Test/Exp/ChooseStar {

    @test
    def testChooseStarOne01(): Bool =
        let f = x -> {
            choose* x {
                case Absent     => Absent
                case Present(v) => Present(v)
            }
        };
        let isAbsent = x -> choose x {
            case Absent => true
        };
        f(Absent) |> isAbsent

    @test
    def testChooseStarOne02(): Bool =
        let f = x -> {
            choose* x {
                case Absent     => Absent
                case Present(v) => Present(v)
            }
        };
        let isPresent = x -> choose x {
            case Present(_) => true
        };
        f(Present(123)) |> isPresent

    @test
    def testChooseStarOne03(): Bool =
        let f = x -> {
            choose* x {
                case Absent     => Present(123)
                case Present(_) => Absent
            }
        };
        let isAbsent = x -> choose x {
            case Absent => true
        };
        f(Present(123)) |> isAbsent

    @test
    def testChooseStarOne04(): Bool =
        let f = x -> {
            choose* x {
                case Absent     => Present(123)
                case Present(_) => Absent
            }
        };
        let isPresent = x -> choose x {
            case Present(_) => true
        };
        f(Absent) |> isPresent

    @test
    def testChooseStarOTwo01(): Bool =
        let f = (x, y) -> {
            choose* (x, y) {
                case (Absent, Absent)         => Absent
                case (Present(_), Present(_)) => Present(123)
            }
        };
        let isAbsent = x -> choose x {
            case Absent => true
        };
        f(Absent, Absent) |> isAbsent

    @test
    def testChooseStarOTwo02(): Bool =
        let f = (x, y) -> {
            choose* (x, y) {
                case (Absent, Absent)         => Absent
                case (Present(_), Present(_)) => Present(123)
            }
        };
        let isPresent = x -> choose x {
            case Present(_) => true
        };
        f(Present(123), Present(456)) |> isPresent

    @test
    def testMap01(): Bool =
        let map = (f, x) -> {
            choose* x {
                case Absent     => Absent
                case Present(v) => Present(f(v))
            }
        };
        let isPresent = x -> choose x {
            case Present(_) => true
        };
        map(x -> x * 2, Present(123)) |> isPresent

    @test
    def testMap02(): Bool =
        let map2 = (f, x, y) -> {
            choose* (x, y) {
                case (Absent, Absent)           => Absent
                case (Absent, Present(_))       => Absent
                case (Present(_), Absent)       => Absent
                case (Present(v1), Present(v2)) => Present(f(v1, v2))
            }
        };
        let isPresent = x -> choose x {
            case Present(_) => true
        };
        map2((x, y) -> x * y, Present(123), Present(456)) |> isPresent

}

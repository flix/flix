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

}

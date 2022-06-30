namespace Test/Exp/Fixpoint/Solve/Lattice {

    enum Constant {
          case Top,

        case Cst(Int32),

          case Bot
    }

    instance LowerBound[Constant] {
        pub def minValue(): Constant = Constant.Bot
    }

    instance Eq[Constant] {
        pub def eq(x: Constant, y: Constant): Bool = match (x, y) {
            case (Top, Top) => true
            case (Cst(a), Cst(b)) => a == b
            case (Bot, Bot) => true
            case _ => false
        }
    }

    instance PartialOrder[Constant] {
        pub def lessEqual(e1: Constant, e2: Constant): Bool = match (e1, e2) {
            case (Bot, _)           => true
            case (Cst(n1), Cst(n2)) => n1 == n2
            case (_, Top)           => true
            case _                  => false
        }
    }

    instance JoinLattice[Constant] {
        pub def leastUpperBound(x: Constant, y: Constant): Constant = match (x, y) {
            case (Bot, _)           => y
            case (_, Bot)           => x
            case (Cst(n1), Cst(n2)) => if (n1 == n2) x else Top
            case _                  => Top
        }
    }

    instance MeetLattice[Constant] {
        pub def greatestLowerBound(e1: Constant, e2: Constant): Constant = match (e1, e2) {
            case (Top, x)           => x
            case (x, Top)           => x
            case (Cst(n1), Cst(n2)) => if (n1 == n2) e1 else Bot
            case _                  => Bot
        }
    }

    instance Order[Constant] {
        pub def compare(x: Constant, y: Constant): Comparison = match (x, y) {
            case (Bot, Bot)         => EqualTo
            case (Bot, Cst(_))      => LessThan
            case (Bot, Top)         => LessThan
            case (Cst(_), Bot)      => GreaterThan
            case (Cst(v1), Cst(v2)) => v1 <=> v2
            case (Cst(_), Top)      => LessThan
            case (Top, Bot)         => GreaterThan
            case (Top, Cst(_))      => GreaterThan
            case (Top, Top)         => EqualTo
        }
    }

    instance Boxable[Constant]

    instance ToString[Constant] {
        pub def toString(x: Constant): String = match x {
            case Top    => "Top"
            case Cst(n) => "Cst(${n})"
            case Bot    => "Bot"
        }
    }

    def sum(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Bot, _)           => Bot
        case (_, Bot)           => Bot
        case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
        case _                  => Top
    }

    @test
    def testFixpointLattice01(): Bool =
        let m = solve #{
            LocalVar((); Top). LocalVar((); Top). LocalVar((); Top). LocalVar((); Top).
        };
        let ans = query m select t from LocalVar(_; t);
        List.length(ans) == 1 and List.head(ans) == Some(Top)

    @test
    def testFixpointLattice02(): Bool =
        let m = solve #{
            LocalVar((); Bot). LocalVar((); Top). LocalVar((); Bot). LocalVar((); Top).
        };
        let ans = query m select t from LocalVar(_; t);
        ans.length == 1 and List.head(ans) == Some(Top)

    @test
    def testFixpointLattice03(): Bool =
        let m = solve #{
            LocalVar((); Cst(39)).
        };
        let ans = query m select t from LocalVar(_; t);
        List.length(ans) == 1 and List.head(ans) == Some(Cst(39))

    @test
    def testFixpointLattice04(): Bool =
        let m = solve #{
            LocalVar((); Cst(39)). LocalVar((); Bot).
        };
        let ans = query m select t from LocalVar(_; t);
        List.length(ans) == 1 and List.head(ans) == Some(Cst(39))

    @test
    def testFixpointLattice05(): Bool =
        let m = solve #{
            LocalVar((); Cst(39)). LocalVar((); Cst(39)).
        };
        let ans = query m select t from LocalVar(_; t);
        List.length(ans) == 1 and List.head(ans) == Some(Cst(39))

    @test
    def testFixpointLattice06(): Bool =
        let m = solve #{
            LocalVar((); Cst(39)). LocalVar((); Cst(12)).
        };
        let ans = query m select t from LocalVar(_; t);
        List.length(ans) == 1 and List.head(ans) == Some(Top)

    @test
    def testFixpointLattice07(): Bool =
        let m = solve #{
            LocalVar((); Cst(39)). LocalVar((); Cst(12)). LocalVar((); Bot).
        };
        let ans = query m select t from LocalVar(_; t);
        List.length(ans) == 1 and List.head(ans) == Some(Top)

    @test
    def testFixpointLattice08(): Bool =
        let m = solve #{
            LitStm("a", 39).
            LocalVar(x; Cst(c)) :- LitStm(x, c).
        };
        let ans = query m select t from LocalVar(_; t);
        List.length(ans) == 1 and List.head(ans) == Some(Cst(39))

    @test
    def testFixpointLattice09(): Bool =
        let m = solve #{
            LitStm("a", 39). LitStm("a", 345).
            LocalVar(x; Cst(c)) :- LitStm(x, c).
        };
        let ans = query m select t from LocalVar(_; t);
        List.length(ans) == 1 and List.head(ans) == Some(Top)

    @test
    def testFixpointLattice10(): Bool =
        let m = solve #{
            LitStm("a", 39).
            AddStm("r", "a", "a").
            LocalVar(x; Cst(c)) :- LitStm(x, c).
            LocalVar(r; sum(v1, v2)) :- AddStm(r, x, y), LocalVar(x; v1), LocalVar(y; v2).

        };
        let ans = query m select t from LocalVar("r"; t);
        List.length(ans) == 1 and List.head(ans) == Some(Cst(39+39))

    @test
    def testFixpointLattice11(): Bool =
        let m = solve #{
            LitStm("a", 39).
            LitStm("b", 12).
            AddStm("r", "a", "b").
            LocalVar(x; Cst(c)) :- LitStm(x, c).
            LocalVar(r; sum(v1, v2)) :- AddStm(r, x, y), LocalVar(x; v1), LocalVar(y; v2).
        };
        let ans = query m select t from LocalVar("r"; t);
        List.length(ans) == 1 and List.head(ans) == Some(Cst(39+12))

}

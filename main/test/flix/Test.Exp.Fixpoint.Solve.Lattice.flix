mod Test.Exp.Fixpoint.Solve.Lattice {

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
            case (Constant.Top, Constant.Top) => true
            case (Constant.Cst(a), Constant.Cst(b)) => a == b
            case (Constant.Bot, Constant.Bot) => true
            case _ => false
        }
    }

    instance PartialOrder[Constant] {
        pub def lessEqual(e1: Constant, e2: Constant): Bool = match (e1, e2) {
            case (Constant.Bot, _)           => true
            case (Constant.Cst(n1), Constant.Cst(n2)) => n1 == n2
            case (_, Constant.Top)           => true
            case _                  => false
        }
    }

    instance JoinLattice[Constant] {
        pub def leastUpperBound(x: Constant, y: Constant): Constant = match (x, y) {
            case (Constant.Bot, _)           => y
            case (_, Constant.Bot)           => x
            case (Constant.Cst(n1), Constant.Cst(n2)) => if (n1 == n2) x else Constant.Top
            case _                  => Constant.Top
        }
    }

    instance MeetLattice[Constant] {
        pub def greatestLowerBound(e1: Constant, e2: Constant): Constant = match (e1, e2) {
            case (Constant.Top, x)           => x
            case (x, Constant.Top)           => x
            case (Constant.Cst(n1), Constant.Cst(n2)) => if (n1 == n2) e1 else Constant.Bot
            case _                  => Constant.Bot
        }
    }

    instance Order[Constant] {
        pub def compare(x: Constant, y: Constant): Comparison = match (x, y) {
            case (Constant.Bot, Constant.Bot)         => Comparison.EqualTo
            case (Constant.Bot, Constant.Cst(_))      => Comparison.LessThan
            case (Constant.Bot, Constant.Top)         => Comparison.LessThan
            case (Constant.Cst(_), Constant.Bot)      => Comparison.GreaterThan
            case (Constant.Cst(v1), Constant.Cst(v2)) => v1 <=> v2
            case (Constant.Cst(_), Constant.Top)      => Comparison.LessThan
            case (Constant.Top, Constant.Bot)         => Comparison.GreaterThan
            case (Constant.Top, Constant.Cst(_))      => Comparison.GreaterThan
            case (Constant.Top, Constant.Top)         => Comparison.EqualTo
        }
    }

    instance ToString[Constant] {
        pub def toString(x: Constant): String = match x {
            case Constant.Top    => "Constant.Top"
            case Constant.Cst(n) => "Constant.Cst(${n})"
            case Constant.Bot    => "Constant.Bot"
        }
    }

    def sum(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Constant.Bot, _)           => Constant.Bot
        case (_, Constant.Bot)           => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 + n2)
        case _                  => Constant.Top
    }

    @Test
    def testFixpointLattice01(): Bool =
        let m = solve #{
            LocalVar((); Constant.Top). LocalVar((); Constant.Top). LocalVar((); Constant.Top). LocalVar((); Constant.Top).
        };
        let ans = query m select t from LocalVar(_; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Top)

    @Test
    def testFixpointLattice02(): Bool =
        let m = solve #{
            LocalVar((); Constant.Bot). LocalVar((); Constant.Top). LocalVar((); Constant.Bot). LocalVar((); Constant.Top).
        };
        let ans = query m select t from LocalVar(_; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Top)

    @Test
    def testFixpointLattice03(): Bool =
        let m = solve #{
            LocalVar((); Constant.Cst(39)).
        };
        let ans = query m select t from LocalVar(_; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Cst(39))

    @Test
    def testFixpointLattice04(): Bool =
        let m = solve #{
            LocalVar((); Constant.Cst(39)). LocalVar((); Constant.Bot).
        };
        let ans = query m select t from LocalVar(_; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Cst(39))

    @Test
    def testFixpointLattice05(): Bool =
        let m = solve #{
            LocalVar((); Constant.Cst(39)). LocalVar((); Constant.Cst(39)).
        };
        let ans = query m select t from LocalVar(_; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Cst(39))

    @Test
    def testFixpointLattice06(): Bool =
        let m = solve #{
            LocalVar((); Constant.Cst(39)). LocalVar((); Constant.Cst(12)).
        };
        let ans = query m select t from LocalVar(_; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Top)

    @Test
    def testFixpointLattice07(): Bool =
        let m = solve #{
            LocalVar((); Constant.Cst(39)). LocalVar((); Constant.Cst(12)). LocalVar((); Constant.Bot).
        };
        let ans = query m select t from LocalVar(_; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Top)

    @Test
    def testFixpointLattice08(): Bool =
        let m = solve #{
            LitStm("a", 39).
            LocalVar(x; Constant.Cst(c)) :- LitStm(x, c).
        };
        let ans = query m select t from LocalVar(_; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Cst(39))

    @Test
    def testFixpointLattice09(): Bool =
        let m = solve #{
            LitStm("a", 39). LitStm("a", 345).
            LocalVar(x; Constant.Cst(c)) :- LitStm(x, c).
        };
        let ans = query m select t from LocalVar(_; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Top)

    @Test
    def testFixpointLattice10(): Bool =
        let m = solve #{
            LitStm("a", 39).
            AddStm("r", "a", "a").
            LocalVar(x; Constant.Cst(c)) :- LitStm(x, c).
            LocalVar(r; sum(v1, v2)) :- AddStm(r, x, y), LocalVar(x; v1), LocalVar(y; v2).

        };
        let ans = query m select t from LocalVar("r"; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Cst(39+39))

    @Test
    def testFixpointLattice11(): Bool =
        let m = solve #{
            LitStm("a", 39).
            LitStm("b", 12).
            AddStm("r", "a", "b").
            LocalVar(x; Constant.Cst(c)) :- LitStm(x, c).
            LocalVar(r; sum(v1, v2)) :- AddStm(r, x, y), LocalVar(x; v1), LocalVar(y; v2).
        };
        let ans = query m select t from LocalVar("r"; t);
        Vector.length(ans) == 1 and Vector.head(ans) == Some(Constant.Cst(39+12))

}

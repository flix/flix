namespace Test/Exp/Fixpoint/Solve/Lattice {

    enum Constant {
          case Top,

        case Cst(Int),

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

    instance Boxable[Constant] {
        pub def box(x: Constant): Boxed = {
            let value = x as ##java.lang.Object;
            let compare = (o1, o2) -> Order.compare(o1 as Constant, o2 as Constant);
            let toString = o -> ToString.toString(o as Constant);
            BoxedObject(value, compare, toString)
        }
        pub def unbox(x: Boxed): Constant= match x {
            case BoxedObject(v, _, _) => v as Constant
            case _                    => ?bug
        }
    }

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

    // TODO: How to extract lattice facts?

    @test
    def testFixpointLattice01(): Bool =
        let _m = solve #{
            LocalVar((); Top). LocalVar((); Top). LocalVar((); Top). LocalVar((); Top).
        };
        true // TODO

    @test
    def testFixpointLattice02(): Bool =
        let _m = solve #{
            LocalVar((); Top). LocalVar((); Top). LocalVar((); Top). LocalVar((); Top).
        };
        true // TODO

    @test
    def testFixpointLattice03(): Bool =
        let _m = solve #{
            LocalVar((); Cst(39)).
        };
        true // TODO

    @test
    def testFixpointLattice04(): Bool =
        let _m = solve #{
            LocalVar((); Cst(39)).
        };
        true // TODO

    @test
    def testFixpointLattice05(): Bool =
        let _m = solve #{
            LocalVar((); Cst(39)). LocalVar((); Cst(39)).
        };
        true // TODO

    @test
    def testFixpointLattice06(): Bool =
        let _m = solve #{
            LocalVar((); Cst(39)). LocalVar((); Cst(12)).
        };
        true // TODO

    @test
    def testFixpointLattice07(): Bool =
        let _m = solve #{
            LocalVar((); Cst(39)). LocalVar((); Cst(12)).
        };
        true // TODO

    @test
    def testFixpointLattice08(): Bool =
        let _m = solve #{
            LitStm("a", 39).
            LocalVar(x; Cst(c)) :- LitStm(x, c).
        };
        true // TODO

    @test
    def testFixpointLattice09(): Bool =
        let _m = solve #{
            LitStm("a", 39).
            LocalVar(x; Cst(c)) :- LitStm(x, c).
        };
        true // TODO

    @test
    def testFixpointLattice10(): Bool =
        let _m = solve #{
            LitStm("a", 39).
            AddStm("r", "a", "a").
            LocalVar(x; Cst(c)) :- LitStm(x, c).
            LocalVar(r; sum(v1, v2)) :- AddStm(r, x, y), LocalVar(x; v1), LocalVar(y; v2).

        };
        true // TODO

    @test
    def testFixpointLattice11(): Bool =
        let _m = solve #{
            LitStm("a", 39).
            AddStm("r", "a", "b").
            LocalVar(x; Cst(c)) :- LitStm(x, c).
            LocalVar(r; sum(v1, v2)) :- AddStm(r, x, y), LocalVar(x; v1), LocalVar(y; v2).

        };
        true // TODO

}

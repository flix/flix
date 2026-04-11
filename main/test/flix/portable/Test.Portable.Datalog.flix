mod Test.Portable.Datalog {

    def assertEq(expected: a, actual: a, msg: String): Unit with Eq[a], ToString[a] =
        if (expected == actual) () else bug!("${msg}\nExpected: ${expected}\nActual:   ${actual}")

    enum Constant {
        case Top
        case Cst(Int32)
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
            case (Constant.Bot, _) => true
            case (Constant.Cst(n1), Constant.Cst(n2)) => n1 == n2
            case (_, Constant.Top) => true
            case _ => false
        }
    }

    instance JoinLattice[Constant] {
        pub def leastUpperBound(x: Constant, y: Constant): Constant = match (x, y) {
            case (Constant.Bot, _) => y
            case (_, Constant.Bot) => x
            case (Constant.Cst(n1), Constant.Cst(n2)) => if (n1 == n2) x else Constant.Top
            case _ => Constant.Top
        }
    }

    instance MeetLattice[Constant] {
        pub def greatestLowerBound(e1: Constant, e2: Constant): Constant = match (e1, e2) {
            case (Constant.Top, x) => x
            case (x, Constant.Top) => x
            case (Constant.Cst(n1), Constant.Cst(n2)) => if (n1 == n2) e1 else Constant.Bot
            case _ => Constant.Bot
        }
    }

    instance Order[Constant] {
        pub def compare(x: Constant, y: Constant): Comparison = match (x, y) {
            case (Constant.Bot, Constant.Bot) => Comparison.EqualTo
            case (Constant.Bot, Constant.Cst(_)) => Comparison.LessThan
            case (Constant.Bot, Constant.Top) => Comparison.LessThan
            case (Constant.Cst(_), Constant.Bot) => Comparison.GreaterThan
            case (Constant.Cst(v1), Constant.Cst(v2)) => v1 <=> v2
            case (Constant.Cst(_), Constant.Top) => Comparison.LessThan
            case (Constant.Top, Constant.Bot) => Comparison.GreaterThan
            case (Constant.Top, Constant.Cst(_)) => Comparison.GreaterThan
            case (Constant.Top, Constant.Top) => Comparison.EqualTo
        }
    }

    instance ToString[Constant] {
        pub def toString(x: Constant): String = match x {
            case Constant.Top => "Constant.Top"
            case Constant.Cst(n) => "Constant.Cst(${n})"
            case Constant.Bot => "Constant.Bot"
        }
    }

    @Test
    def recursiveJoins01(): Unit =
        let db = inject List#{(1, 2), (2, 3), (4, 5), (5, 6)} into Edge/2;
        let pr = #{
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let actual = query db, pr select (x, y) from Path(x, y) |> Vector.toSet;
        let expected = Set#{(1, 2), (1, 3), (2, 3), (4, 5), (4, 6), (5, 6)};
        assertEq(expected, actual, "recursiveJoins01 failed.")

    @Test
    def negation01(): Unit =
        let actual = query foodDb() select f from Healthy(f), not Food(f) |> Vector.toSet;
        let expected = Set#{"Water"};
        assertEq(expected, actual, "negation01 failed.")

    @Test
    def provenance01(): Unit =
        let db = #{
            Edge("a", "b", 3).
            Edge("b", "c", 1).
            Edge("c", "d", 2).
        };
        let pr = #{
            Path(x, y) :- Edge(x, y, _).
            Path(x, z) :- Path(x, y), Edge(y, z, _).
        };
        let pp = pquery db, pr select Path("a", "d") with {Edge};
        let actual = Vector.sumWith(v -> ematch v {
            case Edge(_, _, c) => c
        }, pp);
        assertEq(6, actual, "provenance01 failed.")

    @Test
    def lattice01(): Unit =
        let db1 = inject Vector#{1, 2, 3, 4} into A/1;
        let db2 = inject Vector#{2, 3, 4, 5} into B/1;
        let pr = #{
            C(;Set#{x}) :- A(x).
            D(;Set#{x}) :- B(x).
            R(;x) :- C(;x), D(;x).
        };
        let actual = query db1, db2, pr select x from R(;x);
        let expected = Vector#{Set#{2, 3, 4}};
        assertEq(expected, actual, "lattice01 failed.")

    def foodDb(): #{Food(String), Fastfood(String), Healthy(String), Snack(String), Vegetable(String) | r} = #{
        Fastfood("Banana").
        Fastfood("Burger").
        Fastfood("Pizza").

        Snack("Banana").
        Snack("Carrot").
        Snack("Grapes").

        Vegetable("Apple").
        Vegetable("Carrot").
        Vegetable("Grapes").
        Vegetable("Potato").

        Healthy("Water").
        Healthy(x) :- Vegetable(x).

        Food(x) :- Fastfood(x).
        Food(x) :- Snack(x).
        Food(x) :- Vegetable(x).
    }

    pub def runAll(): Unit = {
        let _ = recursiveJoins01();
        let _ = negation01();
        let _ = provenance01();
        let _ = lattice01();
        ()
    }

}

mod Test.Exp.Fixpoint.Solve.Lattice {

    use Assert.assertEq

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
    def testFixpointLattice01(): Unit \ Assert =
        let m = solve #{
            LocalVar((); Constant.Top). LocalVar((); Constant.Top). LocalVar((); Constant.Top). LocalVar((); Constant.Top).
        };
        let r = query m select t from LocalVar(_; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Top), Vector.head(r))

    @Test
    def testFixpointLattice02(): Unit \ Assert =
        let m = solve #{
            LocalVar((); Constant.Bot). LocalVar((); Constant.Top). LocalVar((); Constant.Bot). LocalVar((); Constant.Top).
        };
        let r = query m select t from LocalVar(_; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Top), Vector.head(r))

    @Test
    def testFixpointLattice03(): Unit \ Assert =
        let m = solve #{
            LocalVar((); Constant.Cst(39)).
        };
        let r = query m select t from LocalVar(_; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Cst(39)), Vector.head(r))

    @Test
    def testFixpointLattice04(): Unit \ Assert =
        let m = solve #{
            LocalVar((); Constant.Cst(39)). LocalVar((); Constant.Bot).
        };
        let r = query m select t from LocalVar(_; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Cst(39)), Vector.head(r))

    @Test
    def testFixpointLattice05(): Unit \ Assert =
        let m = solve #{
            LocalVar((); Constant.Cst(39)). LocalVar((); Constant.Cst(39)).
        };
        let r = query m select t from LocalVar(_; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Cst(39)), Vector.head(r))

    @Test
    def testFixpointLattice06(): Unit \ Assert =
        let m = solve #{
            LocalVar((); Constant.Cst(39)). LocalVar((); Constant.Cst(12)).
        };
        let r = query m select t from LocalVar(_; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Top), Vector.head(r))

    @Test
    def testFixpointLattice07(): Unit \ Assert =
        let m = solve #{
            LocalVar((); Constant.Cst(39)). LocalVar((); Constant.Cst(12)). LocalVar((); Constant.Bot).
        };
        let r = query m select t from LocalVar(_; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Top), Vector.head(r))

    @Test
    def testFixpointLattice08(): Unit \ Assert =
        let m = solve #{
            LitStm("a", 39).
            LocalVar(x; Constant.Cst(c)) :- LitStm(x, c).
        };
        let r = query m select t from LocalVar(_; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Cst(39)), Vector.head(r))

    @Test
    def testFixpointLattice09(): Unit \ Assert =
        let m = solve #{
            LitStm("a", 39). LitStm("a", 345).
            LocalVar(x; Constant.Cst(c)) :- LitStm(x, c).
        };
        let r = query m select t from LocalVar(_; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Top), Vector.head(r))

    @Test
    def testFixpointLattice10(): Unit \ Assert =
        let m = solve #{
            LitStm("a", 39).
            AddStm("r", "a", "a").
            LocalVar(x; Constant.Cst(c)) :- LitStm(x, c).
            LocalVar(r; sum(v1, v2)) :- AddStm(r, x, y), LocalVar(x; v1), LocalVar(y; v2).

        };
        let r = query m select t from LocalVar("r"; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Cst(39+39)), Vector.head(r))

    @Test
    def testFixpointLattice11(): Unit \ Assert =
        let m = solve #{
            LitStm("a", 39).
            LitStm("b", 12).
            AddStm("r", "a", "b").
            LocalVar(x; Constant.Cst(c)) :- LitStm(x, c).
            LocalVar(r; sum(v1, v2)) :- AddStm(r, x, y), LocalVar(x; v1), LocalVar(y; v2).
        };
        let r = query m select t from LocalVar("r"; t);
        assertEq(expected = 1, Vector.length(r));
        assertEq(expected = Some(Constant.Cst(39+12)), Vector.head(r))

    @Test
    def testFixpointLattice12(): Unit \ Assert =
        let pr = #{
            A(;1).
            B(;2).
            R(;x) :- A(;x), B(;x).
        };
        let r = query pr select x from R(;x);
        assertEq(expected = Vector#{1}, r)

    @Test
    def testFixpointLattice13(): Unit \ Assert =
        let pr = #{
            A(;2).
            B(;1).
            R(;x) :- A(;x), B(;x).
        };
        let r = query pr select x from R(;x);
        assertEq(expected = Vector#{1}, r)

    @Test
    def testFixpointLattice14(): Unit \ Assert =
        let pr = #{
            A(;1).
            B(;2).
            C(4).
            C(5).
            R(y;x) :- A(;x), B(;x), C(y).
        };
        let r = query pr select (y, x) from R(y; x);
        assertEq(expected = Vector#{(4, 1), (5, 1)}, r)

    @Test
    def testFixpointLattice15(): Unit \ Assert =
        let pr = #{
            A(;1).
            A(;2).
        };
        let r = query pr select x from A(;x);
        assertEq(expected = Vector#{2}, r)

    @Test
    def testFixpointLattice16(): Unit \ Assert =
        let pr = #{
            A(1).
            A(2).
            R(;x) :- A(x).
        };
        let r = query pr select x from R(;x);
        assertEq(expected = Vector#{2}, r)

    @Test
    def testFixpointLattice17(): Unit \ Assert =
        let pr = #{
            A(1).
            A(2).
            R(;x) :- A(x).
        };
        let r = query pr select x from R(;x);
        assertEq(expected = Vector#{2}, r)

    @Test
    def testFixpointLattice18(): Unit \ Assert =
        let pr = #{
            A(1).
            A(2).
            A(3).
            R(;Set#{x}) :- A(x).
        };
        let r = query pr select x from R(;x);
        assertEq(expected = Vector#{Set#{1, 2, 3}}, r)

    @Test
    def testFixpointLattice19(): Unit \ Assert =
        let db1 = inject Vector#{1, 2, 3, 4} into A/1;
        let db2 = inject Vector#{2, 3, 4, 5} into B/1;
        let pr = #{
            C(;Set#{x}) :- A(x).
            D(;Set#{x}) :- B(x).
            R(;x) :- C(;x), D(;x).
        };
        let r = query db1, db2, pr select x from R(;x);
        assertEq(expected = Vector#{Set#{2, 3, 4}}, r)

    @Test
    def testFix01(): Unit \ Assert =
        let pr = #{
            A(1).
            A(2).
            B(;x) :- A(x).
            R(x) :- fix B(;x).
        };
        let r = query pr select x from R(x);
        assertEq(expected = Vector#{2}, r)

    @Test
    def testFix02(): Unit \ Assert =
        let pr = #{
            A(1).
            A(2).
            B(;x) :- A(x).
            R(x) :- fix B(;x).
        };
        let r = query pr select x from R(x);
        assertEq(expected = Vector#{2}, r)

    @Test
    def testFix03(): Unit \ Assert =
        let pr = #{
            A(1).
            A(2).
            B(;x) :- A(x).
            C(x) :- fix B(;x).
            R(x) :- fix C(x).
        };
        let r = query pr select x from R(x);
        assertEq(expected = Vector#{2}, r)

    @Test
    def testFix04(): Unit \ Assert =
        let pr = #{
            A(1).
            A(2).
            B1(;x) :- A(x).
            B2(;x) :- A(x).
            C1(x) :- fix B1(;x).
            C2(x) :- fix B2(;x).
            R(x) :- C1(x), C2(x).
        };
        let r = query pr select x from R(x);
        assertEq(expected = Vector#{2}, r)

    @Test
    def testRelationToLattice01(): Unit \ Assert =
        let pr = #{
            Foo("a", 1).
            Foo("b", 2).
            Bar(n; v) :- Foo(n, v).
        };
        let r = query pr select (x, y) from Bar(x; y);
        assertEq(expected = Vector#{("a", 1), ("b", 2)}, r)

    @Test
    def testRelationToLattice02(): Unit \ Assert =
        let pr = #{
            Foo("a", 1).
            Foo("a", 2).
            Foo("b", 2).
            Foo("b", 4).
            Bar(n; v) :- Foo(n, v).
        };
        let r = query pr select (x, y) from Bar(x; y);
        assertEq(expected = Vector#{("a", 2), ("b", 4)}, r)

}

namespace Stratifier {
    use Int32.max;
    use List.{map, flatMap, partition};

    opaque type Program = List[Constraint]
    opaque type Constraint = (HeadAtom, List[BodyAtom])
    opaque type Atom = PredicateSymbol
    opaque type HeadAtom = Atom
    enum BodyAtom {
        case Positive(Atom)
        case Negative(Atom)
    }
    opaque type PredicateSymbol with Boxable, ToString, Eq, Order = String

    // A :- B leads to (A, true, B)
    type alias PrecedenceEdge = (PredicateSymbol, Bool, PredicateSymbol)
    type alias PrecedenceGraph = List[PrecedenceEdge]

    def convertProgram(p: Program): PrecedenceGraph =
        let Program(c) = p;
        flatMap(convertConstraint, c)

    def convertConstraint(c: Constraint): PrecedenceGraph =
        let Constraint(HeadAtom(Atom(a0)), b) = c;
        let unfoldBodyAtom = ba -> match ba {
            case Positive(Atom(ps)) => (true, ps),
            case Negative(Atom(ps)) => (false, ps)
        };
        b |> map(unfoldBodyAtom) |> map(pna -> match pna {case (pn, a) => (a0, pn, a)})

    pub def ullman(p: Program, numberOfPredicates: Int): Option[Map[PredicateSymbol, Int]] & Impure =
        let pg: PrecedenceGraph = convertProgram(p);
        let (pos, neg) = partition(e -> match e {case (_, b, _) => b}, pg);
        let removeBool = triple -> match triple {case (a, _, b) => (a, b)};
        let facts = project map(removeBool, pos), map(removeBool, neg) into PositiveDependencyEdge, NegativeDependencyEdge;
        let rules = #{
            Stratum(pd; 1) :- PositiveDependencyEdge(pd, _). Stratum(pd; 1) :- PositiveDependencyEdge(_, pd).
            Stratum(pd; 1) :- NegativeDependencyEdge(pd, _). Stratum(pd; 1) :- NegativeDependencyEdge(_, pd).
            Stratum(ph; max(pbs, phs)) :- PositiveDependencyEdge(ph, pb), Stratum(pb; pbs), Stratum(ph; phs).
            Stratum(ph; max(pbs + 1, phs)) :-
                NegativeDependencyEdge(ph, pb),
                Stratum(pb; pbs),
                Stratum(ph; phs),
                if pbs <= numberOfPredicates.
                // allow one level of strata above the bound for a stratification check later
        };
        let solution = solve facts, rules;
        let stratified = (query solution select () from Stratum(_; s) where s >= numberOfPredicates + 1) |> Array.isEmpty;
        if (not stratified)
            None
        else
            let mappings = query solution select (pd, s) from Stratum(pd; s);
            Some(Array.toMap(mappings))

    pub def testScenario01(): Unit & Impure =
        println("testScenario01");
        // A :- B, C
        // B :- D
        // C :- not D
        let p = Program(
            Constraint(HeadAtom(Atom(PredicateSymbol("A"))), Positive(Atom(PredicateSymbol("B"))) :: Positive(Atom(PredicateSymbol("C"))) :: Nil) ::
            Constraint(HeadAtom(Atom(PredicateSymbol("B"))), Positive(Atom(PredicateSymbol("D"))) :: Nil) ::
            Constraint(HeadAtom(Atom(PredicateSymbol("C"))), Negative(Atom(PredicateSymbol("D"))) :: Nil) ::
            Nil
        );
        let result = ullman(p, 4);
        let expected = "Map#{PredicateSymbol(A) -> 2, PredicateSymbol(B) -> 1, PredicateSymbol(C) -> 2, PredicateSymbol(D) -> 1}";
        outputString(result, expected) |> println

    pub def testScenario02(): Unit & Impure =
        println("testScenario02");
        // A :- B, C
        // B :- D
        // C :- not D
        // D :- A
        let p = Program(
            Constraint(HeadAtom(Atom(PredicateSymbol("A"))), Positive(Atom(PredicateSymbol("B"))) :: Positive(Atom(PredicateSymbol("C"))) :: Nil) ::
            Constraint(HeadAtom(Atom(PredicateSymbol("B"))), Positive(Atom(PredicateSymbol("D"))) :: Nil) ::
            Constraint(HeadAtom(Atom(PredicateSymbol("C"))), Negative(Atom(PredicateSymbol("D"))) :: Nil) ::
            Constraint(HeadAtom(Atom(PredicateSymbol("D"))), Negative(Atom(PredicateSymbol("A"))) :: Nil) ::
            Nil
        );
        let result = ullman(p, 4);
        let expected = "Not Stratified";
        outputString(result, expected) |> println

    def outputString(value: Option[Map[PredicateSymbol, Int]], expected: String): String =
        let resString = match value {
            case None => "Not Stratified"
            case Some(res) => ToString.toString(res)
        };
        "Output:\n${resString}\nExpected:\n${expected}"
}

def main(_args: Array[String]): Int32 & Impure =
        Stratifier.testScenario01();
        println("");
        Stratifier.testScenario02();
        0

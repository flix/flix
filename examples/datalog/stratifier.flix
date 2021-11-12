namespace Stratifier {
    type alias Program = List[Constraint]
    type alias Constraint = (HeadAtom, List[BodyAtom])
    type alias Atom = PredicateSymbol
    type alias HeadAtom = Atom
    enum BodyAtom {
        case Positive(Atom)
        case Negative(Atom)
    }
    type alias PredicateSymbol = String

    // A :- B leads to (A, true, B)
    type alias PrecedenceEdge = (PredicateSymbol, Bool, PredicateSymbol)
    type alias PrecedenceGraph = List[PrecedenceEdge]

    pub def convertProgram(p: Program): PrecedenceGraph =
        List.flatMap(convertConstraint, p)

    pub def convertConstraint(c: Constraint): PrecedenceGraph =
        use List.map;
        let (a0, b) = c;
        let unfoldBodyAtom = ba -> match ba {case Positive(a) => (true, a), case Negative(a) => (false, a)};
        b |> map(unfoldBodyAtom) |> map(pna -> match pna {case (pn, a) => (a0, pn, a)})

    pub def ullman(pg: PrecedenceGraph, predicates: Int): Option[Map[PredicateSymbol, Int]] & Impure =
        use Int32.max;
        let facts = project pg into DependencyEdge;
        let rules = #{
            Stratum(p; 1) :- DependencyEdge(p, _, _).
            Stratum(p; 1) :- DependencyEdge(_, _, p).
            Stratum(ph; max(pbs, phs)) :- DependencyEdge(ph, true, pb), Stratum(pb; pbs), Stratum(ph; phs).
            Stratum(ph; max(pbs + 1, phs)) :-
                DependencyEdge(ph, false, pb),
                Stratum(pb; pbs),
                Stratum(ph; phs),
                if pbs <= predicates.
                // allow one level of strata above the bound for a stratification check
        };
        let solution = solve facts, rules;
        let stratified = (query solution select () from Stratum(_; s) where s >= predicates + 1) |> Array.isEmpty;
        if (not stratified) None else
        let mappings = query solution select (p, s) from Stratum(p; s);
        Some(Array.toMap(mappings))

    pub def testScenario01(): Unit & Impure =
        println("testScenario01");
        // A :- B, C
        // B :- D
        // C :- not D
        let program =
            (("A"), Positive("B") :: Positive("C") :: Nil) ::
            (("B"), Positive("D") :: Nil) ::
            (("C"), Negative("D") :: Nil) ::
            Nil;
        outputString(ullman(convertProgram(program), 4), "Map#{A -> 2, B -> 1, C -> 2, D -> 1}") |> println

    pub def testScenario02(): Unit & Impure =
        println("testScenario02");
        // A :- B, C
        // B :- D
        // C :- not D
        // D :- A
        let program =
            (("A"), Positive("B") :: Positive("C") :: Nil) ::
            (("B"), Positive("D") :: Nil) ::
            (("C"), Negative("D") :: Nil) ::
            (("D"), Negative("A") :: Nil) ::
            Nil;
        outputString(ullman(convertProgram(program), 4), "Not Stratified") |> println

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

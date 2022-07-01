namespace Stratifier {
    use Int32.max;
    use List.{map, flatMap, partition};
    use Array.toMap;

    enum Program(List[Constraint])
    enum Constraint((HeadAtom, List[BodyAtom]))
    enum Atom(PredicateSymbol)
    enum HeadAtom(Atom)
    enum BodyAtom {
        case Positive(Atom)
        case Negative(Atom)
    }
    enum PredicateSymbol(String) with Boxable, ToString, Eq, Order

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

    pub def ullman(p: Program, numberOfPredicates: Int32): Option[Map[PredicateSymbol, Int32]] & Impure =
        let pg: PrecedenceGraph = convertProgram(p);
        let (pos, neg) = partition(e -> match e {case (_, b, _) => b}, pg);
        let removeBool = triple -> match triple {case (a, _, b) => (a, b)};
        let facts = inject map(removeBool, pos), map(removeBool, neg) into PositiveDependencyEdge, NegativeDependencyEdge;
        let rules = #{
            Stratum(pd; 0) :- PositiveDependencyEdge(pd, _).
            Stratum(pd; 0) :- PositiveDependencyEdge(_, pd).
            Stratum(pd; 0) :- NegativeDependencyEdge(pd, _).
            Stratum(pd; 0) :- NegativeDependencyEdge(_, pd).
            Stratum(ph; max(pbs, phs)) :- PositiveDependencyEdge(ph, pb), Stratum(pb; pbs), Stratum(ph; phs).
            Stratum(ph; max(pbs + 1, phs)) :-
                NegativeDependencyEdge(ph, pb),
                Stratum(pb; pbs),
                Stratum(ph; phs),
                if pbs < numberOfPredicates.
                // allow one level of strata above the bound for a stratification check later
        };
        let solution = solve facts, rules;
        let m = query solution select (pd, s) from Stratum(pd; s) |> toMap;
        let notStratified = Map.exists((_, stratum) -> stratum >= numberOfPredicates, m);
        if (notStratified)
            None
        else
            Some(m)

    pub def example01(): Unit & Impure =
        println("example01");
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
        let rs = match result {
            case None => "Not Stratified"
            case Some(res) => ToString.toString(res)
        };
        "Output:\n${rs}\nExpected:\n${expected}" |> println

    pub def example02(): Unit & Impure =
        println("example02");
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
        let rs = match result {
            case None => "Not Stratified"
            case Some(res) => ToString.toString(res)
        };
        "Output:\n${rs}\nExpected:\nNot Stratified" |> println

    pub def example03(): Unit & Impure =
        println("example03");
        // A :- not B.
        let p = Program(
            Constraint(HeadAtom(Atom(PredicateSymbol("A"))), Negative(Atom(PredicateSymbol("B"))) :: Nil) ::
            Nil
        );
        let result = ullman(p, 2);
        let expected = "Map#{PredicateSymbol(A) -> 1, PredicateSymbol(B) -> 0}";
        let rs = match result {
            case None => "Not Stratified"
            case Some(res) => ToString.toString(res)
        };
        "Output:\n${rs}\nExpected:\n${expected}" |> println
}

def main(): Unit & Impure =
        Stratifier.example01();
        println("");
        Stratifier.example02();
        println("");
        Stratifier.example03()

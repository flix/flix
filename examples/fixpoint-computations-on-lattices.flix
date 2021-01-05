/// We define the elements of the lattice as an enum. The somewhat
/// unorthodox formatting shows the structure of the lattice:
enum Sign {
              case Top,

    case Neg, case Zer, case Pos,

              case Bot
}

/// The smallest lattice element.
instance LowerBound[Sign] {
    def minValue(): Sign = Sign.Bot
}

/// The equality relation on the lattice elements.
instance PreOrder[Sign] {
    def equ(x: Sign, y: Sign): Bool = match (x, y) {
        case (Top, Top) => true
        case (Neg, Neg) => true
        case (Zer, Zer) => true
        case (Pos, Pos) => true
        case (Bot, Bot) => true
        case _ => false
    }
}

/// The hash on lattice elements.
instance Hash[Sign] {
    def hash(x: Sign): Int32 = match x {
        case Top => 1
        case Neg => 2
        case Zer => 3
        case Pos => 4
        case Bot => 5
    }
}

/// The partial order relation on the lattice elements.
instance PartialOrder[Sign] {
    def partialCompare(x: Sign, y: Sign): Bool = match (x, y) {
        case (Bot, _)   => true
        case (Neg, Neg) => true
        case (Zer, Zer) => true
        case (Pos, Pos) => true
        case (_, Top)   => true
        case _          => false
    }
}

/// The least upper bound relation on the lattice elements.
instance JoinLattice[Sign] {
    def leastUpperBound(x: Sign, y: Sign): Sign = match (x, y) {
        case (Bot, _)   => y
        case (_, Bot)   => x
        case (Neg, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Pos, Pos) => Pos
        case _          => Top
    }
}

/// The greatest lower bound relation on the lattice elements.
instance MeetLattice[Sign] {
    def greatestLowerBound(e1: Sign, e2: Sign): Sign = match (e1, e2) {
        case (Top, x)   => x
        case (x, Top)   => x
        case (Neg, Neg) => Neg
        case (Zer, Zer) => Zer
        case (Pos, Pos) => Pos
        case _          => Bot
    }
}

instance ToString[Sign] {
    def toString(x: Sign): String = match x {
        case Top => "Top"
        case Neg => "Neg"
        case Zer => "Zer"
        case Pos => "Pos"
        case Bot => "Bot"
    }
}

/// We can now declare three map lattices:
lat A(x: String, s: Sign)
lat B(x: String, s: Sign)
lat R(x: String, s: Sign)

/// We can write facts with lattice semantics:
A("a"; Pos).
B("a"; Top).
A("b"; Neg).

/// And rules with lattice semantics:
R("c"; s) :- A("a"; s).
R("c"; s) :- A("b"; s).
R("d"; s) :- A(x; s), B(x; s).

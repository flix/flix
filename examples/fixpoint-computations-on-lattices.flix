/// We define the elements of the lattice as an enum. The somewhat
/// unorthodox formatting shows the structure of the lattice:
enum Sign {
              case Top,

    case Neg, case Zer, case Pos,

              case Bot
}

/// Next, we define all the components that constitute the lattice:

/// The equality relation on the lattice elements.
def equ(e1: Sign, e2: Sign): Bool = e1 == e2

/// The partial order relation on the lattice elements.
def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) {
    case (Bot, _)   => true
    case (Neg, Neg) => true
    case (Zer, Zer) => true
    case (Pos, Pos) => true
    case (_, Top)   => true
    case _          => false
}

/// The least upper bound relation on the lattice elements.
def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) {
    case (Bot, x)   => x
    case (x, Bot)   => x
    case (Neg, Neg) => Neg
    case (Zer, Zer) => Zer
    case (Pos, Pos) => Pos
    case _          => Top
}

/// The greatest lower bound relation on the lattice elements.
def glb(e1: Sign, e2: Sign): Sign = match (e1, e2) {
    case (Top, x)   => x
    case (x, Top)   => x
    case (Neg, Neg) => Neg
    case (Zer, Zer) => Zer
    case (Pos, Pos) => Pos
    case _          => Bot
}

/// Flix does not yet support type classes, so we use this ugly syntax
/// to tell Flix that these functions define a lattice on Sign.
// TODO: Replace of this with one instance of Lattice or similiar?
let Sign<> = (Bot, Top, equ, leq, lub, glb)

instance LowerBound[Sign] {
    def minValue(): Sign = Sign.Bot
}

instance PreOrder[Sign] {
    def equ(x: Sign, y: Sign): Bool = equ(x, y)
}

instance PartialOrder[Sign] {
    def partialCompare(x: Sign, y: Sign): Bool = leq(x, y)
}

instance JoinLattice[Sign] {
    def lub(x: Sign, y: Sign): Sign = lub(x, y)
}

instance MeetLattice[Sign] {
    def glb(x: Sign, y: Sign): Sign = glb(x, y)
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
